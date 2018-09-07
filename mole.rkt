#lang racket
(require "lang/kara.rkt"
         "types.rkt"
         racket/hash)
(provide (all-defined-out))

(def (no-fail)
  ;; The failure continuation that you're
  ;; confident not gonna be called.
  (error "I did not expect this to fail!"))

(def mole%
  (class* object% (writable<%>)
    (super-new)  ; Initializer

;;; Fields
    (def data '?DATA)     ; '?DATA | constructor | Forall

    (def dic (make-hasheq))

    (def sync-ls
      ;; The sync list is synced among its items,
      ;; and everything is synced with itself.
      (list this))

    (def no-sync
      ;; The list of items we cannot sync with
      ;; it is NOT synced among the items of `sync-ls`.
      (list))

;;; Getters
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

;;; Setters
    (define/public (set-data val)
      (match* (data val (get-roles))
        [(_ '?DATA _)
         ;; No new information
         (void)]

        [(_ _ (list))
         ;; Can update
         (set! data val)]

        [(_ _ _)
         ;; Illegal state
         (error "A composite cannot have data")]))

    (define/public (set-sync-ls ls fail-con)
      ;; Note that this method can fail.
      (check-pred list? ls
                  "Sync list must be non-empty")

      (cond [(set-empty? (set-intersect no-sync
                                        ls))
             (set! sync-ls ls)]
            [else (fail-con)]))

    (define/public (add-kid role mole)
      (check-pred symbol? role
                  "Role is a symbol")
      (check-class mole mole%
                   "Kid is a molecule")
      (check-eq? data '?DATA
                 "This is not an atom")

      (hash-set! dic role mole))

    (define/public (set-no-sync ls)
      ;; This method is intended to be used
      ;; for setting up new molecules as proof goals,
      (if (set-empty? (set-intersect ls sync-ls))
          (set! no-sync ls)
          (error "SET-NO-SYNC -- Illegal state" sync-ls ls)))


    (define/public (check-descendant? mole)
      (let loop ([ks  (get-kids)])
        (match ks
          [(list)  #f]

          [(cons kfocus krest)
           (match (send kfocus get-kids)
             [(list)  (or (eq? mole kfocus)
                         (loop krest))]
             [_  (or (send kfocus check-descendant? mole)
                    (loop krest))])])))


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

    (define/public (get-repr-env [env   (make-hasheq)]
                                 [count 65])
      ;; Sort out which variables are
      ;; represented by which symbol. (mutates `env`)
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
           [_
            ;; Already bound
            (void)])]

        ['ATOM  (void)]

        [(or 'KNOWN-STRUCT
            'UNKNOWN-STRUCT)
         (recur (lam (role kid)
                  (match (send kid get-repr-env env count)
                    [(cons _ c)
                     (set! count c)])))])

      ;; Finally, returns the modified environment
      (cons env count))

    (define/public (repr-core env)
      ;; `env`: sync list -> name entry
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

    (define/public (custom-write port)
      ;; Also specifies the how the Repl prints
      (display (repr) port))

    (define/public (refr role)
      ;; Like `ref`, but reference a direct kid
      (hash-ref dic
                role
                'NOT-FOUND))

    (define/public (ref path)
      ;; Reference a descendant.
      (if (null? path)
          this
          (match (refr (car path))
            ['NOT-FOUND 'NOT-FOUND]

            [kid (send kid ref (cdr path))])))

    (define/public (ref-data path)
      ;; Reference the data of a kid.
      (match (ref path)
        ['NOT-FOUND '?DATA]

        [kid (send kid get-data)]))

    (define/public (refr-data path)
      (match (refr path)
        ['NOT-FOUND '?DATA]

        [kid (send kid get-data)]))

    (def (inform task)
      ;; Tell those in the sync list to do something.
      ;; `task` is a function takes a molecule.
      (check-pred procedure? task)
      (for-each
       (lam (subject)
         (task subject))
       (remq this sync-ls)))

    (define-syntax-rule (recur task)
      ;; Order the kids to do something.
      ;; `task` takes a role and a molecule
      (hash-for-each dic
                     (lam (role kid)
                       (task role kid))))

    (define/public (clone-map)
      ;; Returns a mapping of the originals to their copies,
      ;; without the sync list.
      ;; By 'the originals', I mean every single node in the tree.
      (let ([the-map
             ;; holds the result.
             (make-hasheq)]
            [new-me  (new mole%)])
        (send new-me update data)  ; Clone the data


        (hash-for-each
         ;; Work with the kids
         dic
         (lam (role kid)
           (let* ([cp-res (send kid clone-map)]
                  [lu     (hash-ref cp-res kid)])
             ;; Add the kids for `new-me`
             (send new-me add-kid role lu)
             ;; Change `the-map`
             (hash-union! the-map cp-res))))
        (hash-set!
         ;; Don't forget to map itself.
         the-map this new-me)
        the-map))

    (define/public (copy)
      ;; The complete cloning interface (works for the root)
      ;; This is a major time consumer, be efficient.
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

    (define/public (just-expand path)
      ;; Keep adding new kids
      ;; until the path is exhausted.
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

    (define/public (update val
                           [fail-con no-fail])
      ;; Updating and syncing the data.
      ;; `fail-con`: the function to in case of inconsistency.
      (unless (eq? data val)
        (match data
          ['?DATA
           (set-data val)
           (inform
            (lam (m)
              (send m set-data val)))]

          [_
           ;; Conflict: available data is not equal.
           (fail-con)])))

    (define/public (update-path path
                                val
                                [fail-con no-fail])
      ;; Just a convenience function
      (match (ref path)
        ['NOT-FOUND
         (expand path)
         (send (ref path) update val fail-con)]

        [kid
         (send kid update val fail-con)]))

    (define/public (update-role role
                                val
                                [fail-con no-fail])
      ;; Short-hand for the short-hand that is update-path.
      (update-path (list role)
                   val
                   fail-con))

    (define/public (cascade path)
      ;; Flush the sync list down to the
      ;; new descendants on a path.
      (when (and (not (null? path))
               (> (length sync-ls)
                  1))
        ;; Only work when we have something in the sync list.
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


    (define/public (sync m-other
                         [fail-con no-fail])
      ;; Sync up two molecules that haven't been synced before
      (check-class m-other mole%
                   "Expected a molecule")

      (unless (memq m-other sync-ls)
        ;; Fact: The two sync sets are mutually exclusive
        ;; Start out with syncing data
        (def other-data
          (send m-other get-data))

        (match* (data other-data)
          [('?DATA '?DATA)
           ;; Both can be structures

           (def result
             ;; The control is going to be all over the place.
             (let/ec escape
               (when (or (exists? (lam (m) (check-descendant? m))
                                 (send m-other get-sync-ls))
                        (exists? (lam (m) (send m-other check-descendant? m))
                                 sync-ls))
                 ;; Cycle check
                 (escape 'CYCLE))

               (let ([our-roles    (list->seteq (get-roles))]
                     [their-roles  (list->seteq
                                    (send m-other get-roles))])
                 ;; Alright, no cycle: add the missing kids...
                 (set-for-each
                  ;; ... for us,
                  (set-subtract their-roles
                                our-roles)
                  (lam (role)
                    (update-role role '?DATA)))

                 (set-for-each
                  ;; ... and for the other guy.
                  (set-subtract our-roles
                                their-roles)
                  (lam (role)
                    (send m-other update-role
                      role
                      '?DATA))))

               (let ([merge
                      (append sync-ls
                              (send m-other get-sync-ls))])
                 ;; Establish the syncing among the two roots.
                 ;; Note that we do not cascade, to preserve the
                 ;; symmetry of the following recursive calls.
                 ;; This may fail because of user constraint.
                 (set-sync-ls merge
                              (thunk (escape 'CANT-SYNC)))

                 (inform
                  ;; Since `sync-ls` now also includes molecules on
                  ;; 'the other side', we can inform them in one message.
                  (lam (m)
                    (send m set-sync-ls
                      merge
                      (thunk (escape 'CANT-SYNC))))))

               (recur
                ;; Our work is over: Recursively let all the kids sync.
                (lam (role c)
                  (send c sync
                    (send m-other refr role)
                    (thunk (escape 'CONFLICT)))))))

           (when (memq result
                       '(CONFLICT CANT-SYNC CYCLE))
             (fail-con))]

          [(x x)
           ;; They're consistent atoms
           (void)]

          [('?DATA _)
           ;; `m-other` has new data for us
           (update other-data)]

          [(_ '?DATA)
           ;; We have new data for `m-other`
           (send m-other update data)]

          [(_ _)
           ;; Nope, the data are inconsistent.
           (fail-con)])))

    (define/public (unify m-other
                          [fail-con no-fail])
      ;; Like sync, but the only difference is that
      ;; it doesn't have an impact on `m-other`
      (let ([m-other-clone
             (send m-other copy)])
        (sync m-other fail-con)))

    (define/public (sync-path p1
                              p2
                              [fail-con no-fail])
      ;; Sync two descendants of this molecules.
      ;; If they don't exist, expand them.
      (when (eq? 'NOT-FOUND (ref p1))
        (expand p1))
      (when (eq? 'NOT-FOUND (ref p2))
        (expand p2))
      (send (ref p1) sync
        (ref p2)
        fail-con))


    (define/public (distinguish-paths paths)
      ;; Do not let these molecules sync with each other.
      ;; (note: overwrites existing no-sync policies)
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


(define-syntax update-ctors
  ;; Update the constructor of multiple paths of a molecule.
  (syntax-rules ()
    [(_ mole) (void)]

    [(_ mole (path cval) rest ...)
     (begin
       (send mole update-path
         (pad (path-proc 'path) 'ctor)
         cval)
       (update-ctors mole rest ...))]))
