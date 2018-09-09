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
    (def data '?DATA)  ; '?DATA | constructor
    (def type '?TYPE)  ; '?TYPE | type (stream of constructor)

    (def dic (make-hasheq))

    (def sync-ls
      ;; The sync list is synced among its items,
      ;; and everything is synced with itself.
      (list this))

    (def no-sync
      ;; The list of items this molecule cannot sync with
      ;; it is NOT synced among the items of `sync-ls`.
      (list))

    (def expanded?
      #f)
    (def no-touch?
      ;; if set to #t, will return failure
      ;; when we try to update data
      #f)

    ;; Getters
    (define/public (get-dic)
      dic)
    (define/public (get-data)
      data)
    (define/public (get-roles)
      (hash-keys dic))
    (define/public (get-kids)
      (hash-values dic))
    (define/public (get-no-sync)
      no-sync)
    (define/public (get-sync-ls)
      sync-ls)
    (define/public (get-type)
      type)

    ;; Setters
    (define/public (set-data val fail-con)
      (match no-touch?
        [#t  (fail-con)]
        [#f  (set! data val)]))

    (define/public (set-type val)
      (set! type val))

    (define/public (set-sync-ls ls fail-con)
      ;; Note that this method can fail.
      (check-pred list? ls
                  "Sync list must be non-empty")

      (cond [(set-empty? (set-intersect no-sync
                                        ls))
             (set! sync-ls ls)]
            [else  (fail-con)]))

    (define/public (mark-expanded)
      (set! expanded? #t))
    (define/public (mark-no-touch)
      (set! no-touch? #t))

    (define/public (get-height)
      (match (get-kids)
        [(list)  0]
        [kids    (add1 (apply max
                         (for/list ([k (get-kids)])
                           (send k get-height))))]))

    (define/public (add-kid role mole)
      (check-pred number? role
                  "Role is a number")
      (check-class mole mole%
                   "Kid is a molecule")

      (hash-set! dic role mole))

    (define/public (set-no-sync ls)
      ;; This method is intended to be used
      ;; for setting up new molecules as proof goals,
      (if (set-empty? (set-intersect ls sync-ls))
          (set! no-sync ls)
          (error "SET-NO-SYNC -- Illegal state" sync-ls ls)))

    (define/public (custom-display port)
      (display (repr) port))

    (define/public (custom-write port)
      (write (cons data
                   (hash->list dic))
             port))

    (define/public (repr)
      (let* ([env  (make-hasheq)])
        (set-repr-env! env
                       (let ([count 64])
                         ;; The counting closure
                         (thunk (set! count (add1 count))
                                (integer->char count))))
        (repr-core env)))

    (def (classify)
      (match* ((get-kids) data)
        [((list) '?DATA)  'BLANK]
        [((list) _)       'ATOM]
        [(_ _)            'STRUCT]))

    (define/public (set-repr-env! env get-next)
      ;; Sort out which variables are
      ;; represented by which symbol.
      (match (classify)
        ['BLANK  (match (hash-ref env
                                  this
                                  'UNBOUND)
                   ['UNBOUND (let ([new-symbol  (get-next)])
                               (for ([m sync-ls])
                                 (hash-set! env
                                            m
                                            new-symbol)))]
                   [_
                    ;; Already bound
                    (void)])]

        ['ATOM  (void)]

        ['STRUCT
         (for ([kid (get-kids)])
           (send kid set-repr-env!
             env get-next))]))

    (define/public (repr-core env)
      (match (classify)
        ['BLANK  (hash-ref env this)]

        ['ATOM   data]

        ['STRUCT
         (let ([leader (match data
                         ['?DATA            '?]
                         [(Ctor ldr _ _ _)  ldr])])
           (cons leader
                 (let ([ceiling
                        (add1 (apply max (get-roles)))])
                   (for/list ([role
                               (range ceiling)])
                     (match (refr role)
                       ['NOT-FOUND  '|.|]
                       [kid
                        (send kid repr-core env)])))))]))

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
      (for ([subject (remq this sync-ls)])
        (task subject)))

    (define/public (clone-map)
      ;; Returns a mapping of the originals to their copies,
      ;; without the sync list.
      ;; By 'the originals', I mean every single node in the tree.
      (let ([the-map
             ;; holds the result.
             (make-hasheq)]
            [new-me  (new mole%)])
        (begin
          ;; Clone the top-level information
          (send new-me set-data data no-fail)
          (send new-me set-type type)
          (when expanded?
            (send new-me mark-expanded))
          (when no-touch?
            (send new-me mark-no-touch)))

        (for ([(role kid) dic])
          ;; Work with the kids
          (let* ([cp-res (send kid clone-map)]
                 [lu     (hash-ref cp-res kid)])
            ;; Add the kids for `new-me`
            (send new-me add-kid role lu)
            ;; Change `the-map`
            (hash-union! the-map cp-res)))

        (hash-set!
         ;; Don't forget to map itself.
         the-map this new-me)

        the-map))

    (define/public (copy)
      ;; The complete cloning interface (works for the root)
      ;; This is a major time consumer, be efficient.
      (let ([cns (clone-map)])
        (for ([(orig clone) cns])
          (send clone set-sync-ls
            ;; Translate the entire sync list
            ;; to the copied version.
            (for/list ([m (send orig get-sync-ls)])
              (hash-ref cns
                        m
                        (thunk
                         (error "sync-ls has non-descendants."
                                this))))
            no-fail)
          (send clone set-no-sync
            ;; Same thing with no-sync
            (for/list ([m (send orig get-no-sync)])
              (hash-ref cns
                        m
                        (thunk
                         (error "no-sync-ls has non-descendants."
                                this))))))

        (hash-ref
         ;; Return the root's copy.
         cns this)))

    (define/public (just-expand path)
      ;; Keep adding new kids
      ;; until the path is exhausted.
      (unless (null? path)
        (let ([new-kid (new mole%)])
          (add-kid (car path) new-kid)
          (send new-kid just-expand (cdr path)))))

    (define/public (expand path)
      (unless (null? path)
        (match (refr (car path))
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
        (match (let/ec escape
                 (match data
                   ['?DATA (set-data val (thunk (escape 'NO-TOUCH)))
                           (inform
                            (lam (m) (send m set-data
                                     val (thunk (escape 'NO-TOUCH)))))]
                   ;; Conflict: available data is not equal.
                   [_      (escape 'CONFLICT)]))

          [(or 'NO-TOUCH 'CONFLICT)  (fail-con)]
          [_                        (void)])))

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
            (for/list ([sy-i sync-ls])
              (send sy-i refr role))
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
        (def result
          ;; The control is going to be all over the place.
          (let/ec escape
            (match* (data (send m-other get-data))
              ;; Syncing the data
              [(x x)        (void)]
              [('?DATA od)  (update od)]
              [(_ '?DATA)   (send m-other update data)]
              [(_ _)        (escape 'CONFLICT)])

            (let* ([our-roles      (list->seteq (get-roles))]
                   [their-roles    (list->seteq
                                    (send m-other get-roles))]
                   ;; Monitor the height to check for cycle
                   [max-height     (thunk (max (get-height)
                                               (send m-other get-height)))]
                   [height-before  (max-height)])
              ;; Add the missing kids...
              (for ([role (set-subtract their-roles
                                        our-roles)])
                ;; ... for us,
                (expand (list role)))

              (for ([role (set-subtract our-roles
                                        their-roles)])
                ;; ... for the other guy.
                (send m-other expand (list role)))

              (when (> (max-height)
                       height-before)
                ;; Remember the cycle check?
                (escape 'CYCLE)))

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

            (for ([(role kid) dic])
              ;; Our work is over: Recursively let all the kids sync.
              ;; And `max-height` will be reduced by 1.
              (send kid sync
                (send m-other refr role)
                (thunk (escape 'FAIL-LOW))))))

        (case result
          [(CONFLICT
            CANT-SYNC
            CYCLE
            FAIL-LOW) (fail-con)]
          [else       (void)])))

    (define/public (unify m-other
                          [fail-con no-fail])
      ;; Like sync, but the only difference is that
      ;; it doesn't have an impact on `m-other`
      (let ([m-other-clone
             (send m-other copy)])
        (sync m-other fail-con)))

    (define/public (expand-and-get-paths paths)
      (for      ([p paths]) (expand p))
      (for/list ([p paths]) (ref p)))

    (define/public (sync-paths paths
                               [fail-con no-fail])
      ;; Sync two or more descendants.
      (check-true (>= (length paths) 2)
                  "sync-paths must be given than one paths")

      (def result
        (let/ec escape
          (let* ([moles    (expand-and-get-paths paths)]
                 [master   (first moles)]
                 [servants (list-tail moles 1)])
            (for ([m servants])
              (send m sync
                master (thunk (escape 'FAILURE)))))))

      (when (eq? result 'FAILURE)
        (fail-con)))

    (define/public (distinguish paths)
      ;; Do not let these molecules sync with each other.
      ;; (note: overwrites existing no-sync policies)
      ;; (note: error checking is left to the user)
      (let ([moles (expand-and-get-paths paths)])
        (for ([m moles])
          (send m set-no-sync (remq m
                                    moles)))))

    (define/public (preserve paths)
      (for ([m (expand-and-get-paths paths)])
        (send m mark-no-touch)))))
