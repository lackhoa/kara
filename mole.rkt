#lang racket
(require "lang/kara.rkt"
         "types.rkt"
         racket/hash)
(provide (all-defined-out))

; ---------------------------------
; Molecules
; ---------------------------------

; The failure continuation that you're
; confident not gonna be called.
(def (no-fail)
  (error "I did not expect this to fail!"))

(def mole%
  (class* object% (writable<%>)
    ;; Initializer
    (super-new)

    ;; Fields
    (def data '?DATA)     ; '?DATA | constructor | Forall

    (def dic (make-hasheq))

    ;; The sync list is synced among its items,
    ;; and everything is synced with itself.
    (def sync-ls
      (list this))

    ;; The list of items this molecule cannot sync with
    ;; it is NOT synced among the items of `sync-ls`.
    (def no-sync (list))

    ;; if set to #t, will return failure
    ;; when we try to update data
    (def no-touch? #f)

    ;; Getters
    (define/public (get-dic)
      dic)
    (define/public (get-data)
      data)
    (define/public (get-roles)
      (hash-keys dic))
    (define/public (get-kids)
      (hash-values dic))


    (define/public (get-sync-ls)
      sync-ls)

    (define/public (get-ctor)
      (refr-data 'ctor))

    (define/public (get-type)
      (refr-data 'type))

    (define/public (ref-ctor path)
      (ref-data (pad path 'ctor)))

    (define/public (ref-type path)
      (ref-data (pad path 'type)))

    ;; Setters
    (define/public (set-data val fail-con)
      (match no-touch?
        [#t  (fail-con)]
        [#f  (match* (data val (get-roles))
               ;; No new information
               [(_ '?DATA _)  (void)]
               ;; Can update
               [(_ _ (list))  (set! data val)]
               ;; Illegal state
               [(_ _ _)       (error "A composite can't have data" this)])]))

    ;; Note that this method can fail.
    (define/public (set-sync-ls ls fail-con)
      (check-pred list? ls
                  "Sync list must be non-empty")

      (cond [(set-empty? (set-intersect no-sync
                                        ls))
             (set! sync-ls ls)]
            [else (fail-con)]))

    (define/public (mark-no-touch)
      (set! no-touch? #t))

    (define/public (get-height)
      (match (get-kids)
        [(list)  0]
        [kids    (add1 (apply max
                         (map (lam (k) (send k get-height))
                              (get-kids))))]))

    (define/public (add-kid role mole)
      (check-pred symbol? role
                  "Role is a symbol")
      (check-class mole mole%
                   "Kid is a molecule")
      (check-eq? data '?DATA
                 "This is not an atom")

      (hash-set! dic role mole))

    ;; This method is intended to be used
    ;; for setting up new molecules as proof goals,
    (define/public (set-no-sync ls)
      (if (set-empty? (set-intersect ls sync-ls))
          (set! no-sync ls)
          (error "SET-NO-SYNC -- Illegal state" sync-ls ls)))


    (define/public (repr)
      (repr-core (car (get-repr-env))))

    (def (classify)
      (match data
        ['?DATA  (match (get-ctor)
                   ['?DATA (match (remq* '(type ctor)
                                         (get-roles))
                             [(list)  'BLANK]
                             [_       'UNKNOWN-STRUCT])]
                   [_  'KNOWN-STRUCT])]
        [_  'ATOM]))

    ;; Sort out which variables are
    ;; represented by which symbol. (mutates `env`)
    (define/public (get-repr-env [env   (make-hasheq)]
                                 [count 65])
      (match (classify)
        ['BLANK
         (match (hash-ref env
                          this
                          'UNBOUND)
           ['UNBOUND (let ([new-symbol
                            (integer->char count)])
                       (for-each (lam (m)
                                   (hash-set! env
                                              m
                                              new-symbol))
                                 sync-ls))
                     (set! count (+ 1 count))]
           ;; Already bound
           [_  (void)])]

        ['ATOM  (void)]

        [(or 'KNOWN-STRUCT
             'UNKNOWN-STRUCT)
         (recur (lam (role kid)
                  (match (send kid get-repr-env env count)
                    [(cons _ c)
                     (set! count c)])))])

      ;; Finally, returns the modified environment
      (cons env count))

    ;; `env`: sync list -> name entry
    (define/public (repr-core env)
      (match (classify)
        ['BLANK
         (hash-ref env
                   this
                   (thunk (error "Repr code is wrong")))]

        ['ATOM  data]

        ['UNKNOWN-STRUCT
         (map (lam (role)
                ;; Display the roles and the kids.
                (cons role
                      (send (refr role) repr-core env)))
              (remq* '(type ctor)
                     (get-roles)))]

        ['KNOWN-STRUCT
         (match (get-ctor)
           [(Ctor ctor-repr _ _ _)
            (match ctor-repr
              [(Repr leader roles)
               (if (null? roles)
                   leader
                   (cons leader
                         (map (lam (role)
                                (match (refr role)
                                  ['NOT-FOUND  '?]
                                  [kid
                                   (send kid repr-core env)]))
                              roles)))])])]))

    (define/public (custom-display port)
      (display (repr) port))

    ;; Also specifies the Repl printing
    (define/public (custom-write port)
      (display (repr) port))

    ;; Like `ref`, but reference a direct kid
    (define/public (refr role)
      (hash-ref dic
                role
                'NOT-FOUND))

    ;; Reference a descendant.
    (define/public (ref path)
      (if (null? path)
          this
          (match (refr (car path))
            ['NOT-FOUND 'NOT-FOUND]

            [kid (send kid ref (cdr path))])))

    ;; Reference the data of a kid.
    (define/public (ref-data path)
      (match (ref path)
        ['NOT-FOUND '?DATA]

        [kid (send kid get-data)]))

    (define/public (refr-data path)
      (match (refr path)
        ['NOT-FOUND '?DATA]

        [kid (send kid get-data)]))

    ;; Tell those in the sync list to do something.
    ;; `task` is a function takes a molecule.
    (def (inform task)
      (check-pred procedure? task)
      (for-each
       (lam (subject)
         (task subject))
       (remq this sync-ls)))

    ;; Order the kids to do something.
    ;; `task` takes a role and a molecule
    (define-syntax-rule (recur task)
      (hash-for-each dic
                     (lam (role kid)
                       (task role kid))))

    ;; Returns a mapping of the originals to their copies,
    ;; without the sync list.
    ;; By 'the originals', I mean every single node in the tree.
    (define/public (clone-map)
      ;; `the-map` holds the result.
      (let ([the-map (make-hasheq)]
            [new-me  (new mole%)])
        ;; Clone the data
        (send new-me update data)
        ;; Clone the information necessary
        (when no-touch?
          (send new-me mark-no-touch))

        ;; Work with the kids
        (hash-for-each dic
                       (lam (role kid)
                         (let* ([cp-res (send kid clone-map)]
                                [lu     (hash-ref cp-res kid)])
                           ;; Add the kids for `new-me`
                           (send new-me add-kid role lu)
                           ;; Change `the-map`
                           (hash-union! the-map cp-res))))
        ;; Don't forget to map itself.
        (hash-set! the-map this new-me)
        the-map))

    ;; The complete cloning interface (works for the root)
    ;; This is a major time consumer, be efficient.
    (define/public (copy)
      (let ([cns (clone-map)])
        (hash-for-each cns
                       (lam (orig clone)
                         ;; Translate the entire sync list
                         ;; to the copied version.
                         (send clone set-sync-ls
                           (map
                            (lam (m)
                              (hash-ref cns
                                        m
                                        (thunk
                                         (error "Sync list contains non-descendants."
                                                this))))
                            (send orig get-sync-ls))
                           no-fail)))
        ;; This will return the root's copy.
        (hash-ref cns this)))

    ;; Keep adding new kids
    ;; until the path is exhausted.
    (define/public (just-expand path)
      (unless (null? path)
        (let ([new-kid (new mole%)])
          (add-kid (car path) new-kid)
          (send new-kid just-expand (cdr path)))))

    (define/public (expand path)
      (let ([lu (refr (car path))])
        (match lu
          ['NOT-FOUND
           (just-expand path)
           (inform (lam (m) (send m just-expand path)))
           ;; The order is important: we must add all
           ;; components to refer to them in the sync list.
           (cascade path)
           (inform (lam (m) (send m cascade path)))]

          [kid
           (send kid expand (cdr path))])))

    ;; Updating and syncing the data.
    ;; `fail-con`: the function to in case of inconsistency.
    (define/public (update val
                           [fail-con no-fail])
      (unless (eq? data val)
        (match (let/ec escape
                 (match data
                   ['?DATA (set-data val (thunk (escape 'NO-TOUCH)))
                           (inform
                            (lam (m) (send m set-data
                                     val (thunk (escape 'NO-TOUCH)))))]
                   ;; Conflict: available data is not equal.
                   [_      (escape 'CONFLICT)]))

          [(or 'NO-TOUCH 'CONFLICT)  (fail-con)]
          [_                         (void)])))

    ;; Just a convenience function
    (define/public (update-path path
                                val
                                [fail-con no-fail])
      (match (ref path)
        ['NOT-FOUND
         (expand path)
         (send (ref path) update val fail-con)]

        [kid
         (send kid update val fail-con)]))

    ;; Short-hand for the short-hand that is update-path.
    (define/public (update-role role
                                val
                                [fail-con no-fail])
      (update-path (list role)
                   val
                   fail-con))

    ;; Flush the sync list down to the
    ;; new descendants on a path.
    (define/public (cascade path)
      ;; Only work when we have something in the sync list.
      (when (and (not (null? path))
                 (> (length sync-ls)
                    1))
        (let* ([role (car path)]
               [c    (refr role)])
          (check-class c mole%
                       "We have this path")
          (check-eqv? (length (send c get-sync-ls))
                      1
                      "This kid is new")
          (send c set-sync-ls
            (map (lam (sy-i) (send sy-i refr role))
                 sync-ls)
            no-fail)
          ;; Then let them carry over.
          (send c cascade (cdr path)))))


    ;; Sync up two molecules that haven't been synced before
    (define/public (sync m-other
                         [fail-con no-fail])
      (check-class m-other mole%
                   "Expected a molecule")

      (unless (memq m-other sync-ls)
        ;; Fact: The two sync sets are mutually exclusive
        ;; Start out with syncing data
        (def other-data
          (send m-other get-data))

        (match* (data other-data)
          ;; Both can be structures
          [('?DATA '?DATA)

           ;; The control is going to be all over the place.
           (def result
             (let/ec escape
               ;; Add the missing kids...
               (let* ([our-roles      (list->seteq (get-roles))]
                      [their-roles    (list->seteq
                                       (send m-other get-roles))]
                      ;; Monitor the height to check for cycle
                      [max-height     (thunk (max (get-height)
                                                  (send m-other get-height)))]
                      [height-before  (max-height)])
                 ;; ... for us,
                 (set-for-each (set-subtract their-roles
                                             our-roles)
                               (lam (role)
                                 (update-role role '?DATA)))

                 ;; ... and for the other guy.
                 (set-for-each (set-subtract our-roles
                                             their-roles)
                               (lam (role)
                                 (send m-other update-role
                                   role '?DATA)))

                 ;; Remember the cycle check?
                 (when (> (max-height)
                          height-before)
                   (escape 'CYCLE)))

               ;; Establish the syncing among the two roots.
               ;; Note that we do not cascade, to preserve the
               ;; symmetry of the following recursive calls.
               ;; This may fail because of user constraint.
               (let ([merge
                      (append sync-ls
                              (send m-other get-sync-ls))])
                 (set-sync-ls merge
                              (thunk (escape 'CANT-SYNC)))

                 ;; Since `sync-ls` now also includes molecules on
                 ;; 'the other side', we can inform them in one message.
                 (inform (lam (m)
                           (send m set-sync-ls
                             merge
                             (thunk (escape 'CANT-SYNC))))))

               ;; Our work is over: Recursively let all the kids sync.
               ;; And `max-height` will be reduced by 1.
               (recur (lam (role c)
                        (send c sync
                          (send m-other refr role)
                          (thunk (escape 'CONFLICT)))))))

           (when (memq result
                       '(CONFLICT CANT-SYNC CYCLE))
             (fail-con))]

          ;; They're consistent atoms
          [(x x) (void)]

          ;; `m-other` has new data for us
          [('?DATA _)
           (update other-data)]

          ;; We have new data for `m-other`
          [(_ '?DATA)
           (send m-other update data)]

          ;; Nope, the data are inconsistent.
          [(_ _) (fail-con)])))


    ;; Like sync, but the only difference is that
    ;; it doesn't have an impact on `m-other`
    (define/public (unify m-other
                          [fail-con no-fail])
      (let ([m-other-clone
             (send m-other copy)])
        (sync m-other fail-con)))

    ;; Sync two descendants of this molecules.
    ;; If they don't exist, expand them.
    (define/public (sync-path p1
                              p2
                              [fail-con no-fail])
      (when (eq? 'NOT-FOUND (ref p1))
        (expand p1))
      (when (eq? 'NOT-FOUND (ref p2))
        (expand p2))
      (send (ref p1) sync
        (ref p2)
        fail-con))


    ;; Do not let these molecules sync with each other.
    ;; (note: overwrites existing no-sync policies)
    (define/public (distinguish-paths paths)
      (let ([moles null])
        (for ([p paths])
          (match (ref p)
            ['NOT-FOUND
             (expand p)
             (cons! (ref p) moles)]

            [m  (cons! m moles)]))

        (for ([m moles])
          (send m set-no-sync (remq m
                                    moles)))))))


;; Update the constructor of multiple paths of a molecule.
(define-syntax update-ctors
  (syntax-rules ()
    [(_ mole) (void)]

    [(_ mole (path cval) rest ...)
     (begin
       (send mole update-path
         (pad (path-proc 'path) 'ctor)
         cval)
       (update-ctors mole rest ...))]))
