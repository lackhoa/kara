(def mol%
  (class* object% (writable<%>)



    (define/public (repr-core mol env)
      (match (classify mol)
        ['BLANK   (hash-ref env mol)]
        ['ATOM    (mol%-data mol)]
        ['STRUCT  (cons (match (mol%-data mol) ['?DATA '?] [any any])
                        (for/list ([role
                                    (range (add1 (apply max (get-roles))) #|The ceiling|#)])
                          (match (refr mol role)
                            ['NOT-FOUND  '-]
                            [kid         (repr-core kid env)])))]))



    (define/public (clone-map)
      ;; Returns a mapping of the originals to their copies,
      ;; without the sync list.
      ;; By 'the originals', I mean every single node in the tree.
      (let ([the-map
             ;; holds the result.
             (make-hasheq)]
            [new-me  (new mol%)])
        (begin
          ;; Clone the top-level information
          (send new-me set-data data no-fail)
          (send new-me set-type type)
          (when expanded?
            (send new-me mark-expanded)))

        (for ([(role kid) dic])
          ;; Work with the kids
          (let* ([cp-res (send kid clone-map)]
                 [lu     (hash-ref cp-res kid)])
            ;; Add the kids for `new-me`
            (send new-me add-kid
              role lu no-fail)
            ;; Change `the-map`
            (hash-union! the-map cp-res)))

        (when no-touch?
          ;; note: this step must be done afterwards,
          ;; otherwise we wouldn't be able to add kids
          (send new-me mark-no-touch))

        (hash-set!
         ;; Don't forget to map itself.
         the-map this new-me)

        the-map))

    (define/public (copy)
      ;; The complete cloning interface
      ;; (clones everything besides non-descendants)
      ;; A major time consumer, be efficient.
      (let ([cns (clone-map)])
        (for ([(orig clone) cns])
          (send clone set-sync-ls
            ;; Translate the sync list
            ;; to the copied version.
            (set-remove (for/list ([m (send orig get-sync-ls)])
                          (hash-ref cns m 'NON-DESCENDANT))
                        'NON-DESCENDANT)
            no-fail)

          (send clone set-no-sync
            ;; Same thing with no-sync
            (set-remove (for/list ([m (send orig get-no-sync)])
                          (hash-ref cns m 'NON-DESCENDANT))
                        'NON-DESCENDANT)))

        (hash-ref
         ;; Return the root's copy.
         cns this)))

    (define/public (expand path
                           [fail-con no-fail])
      (unless (null? path)
        (match (let/ec escape
                 (match (refr (car path))
                   ['NOT-FOUND
                    (just-expand path
                                 (thunk (escape 'FAIL)))
                    (inform (lam (m)
                              (send m just-expand
                                path
                                (thunk (escape 'FAIL)))))
                    ;; The order is important: we must add all
                    ;; components to refer to them in the sync list.
                    (cascade path)
                    (inform (lam (m) (send m cascade path)))]

                   [kid
                    (send kid expand
                      (cdr path)
                      (thunk (escape 'FAIL)))]))
          ['FAIL  (fail-con)]
          [_      (void)])))



    (define/public (cascade path)
      ;; Flush the sync list down to the
      ;; new descendants on a path.
      (when (and (not (null? path))
                 (> (length sync-ls)
                    1))
        ;; Only work when we have something in the sync list.
        (let* ([role (car path)]
               [c    (refr role)])
          (check-class c mol%
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
      ;; Sync up two molcules that haven't been synced before
      (check-class m-other mol%
                   "Expected a molcule")

      (unless (memq m-other sync-ls)
        ;; Fact: The two sync sets are mutually exclusive
        ;; Start out with syncing data
        (match (let/ec escape
                 (match* (data (send m-other get-data))
                   ;; Syncing the data
                   [(x x)        (void)]
                   [('?DATA od)  (update od (thunk (escape 'NO-TOUCH)))]
                   [(_ '?DATA)   (send m-other update
                                   data (thunk (escape 'NO-TOUCH)))]
                   [(_ _)        (escape 'CONFLICT)])

                 (let* ([our-roles      (list->seteq (get-roles))]
                        [their-roles    (list->seteq
                                         (send m-other get-roles))]
                        ;; Monitor the height to check for cycle
                        [max-height     (thunk (max (height this)
                                                    (height m-other)))]
                        [height-before  (max-height)])
                   ;; Add the missing kids...
                   (for ([role (set-subtract their-roles
                                             our-roles)])
                     ;; ... for us,
                     (expand (list role)
                             (thunk (escape 'FAIL-LOW))))

                   (for ([role (set-subtract our-roles
                                             their-roles)])
                     ;; ... for the other guy.
                     (send m-other expand
                       (list role)
                       (thunk (escape 'FAIL-LOW))))

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
                    ;; Since `sync-ls` now also includes molcules on
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
                     (thunk (escape 'FAIL-LOW))))

                 'OK)

          ['OK  (void)]
          [_    (fail-con)])))))

;;; Macros
(define-syntax update-macro
  (syntax-rules ()
    [(_ mol)  (void)]

    [(_ mol (path ctor) rest ...)
     (begin
       (send mol update-path
         'path ctor)
       (update-macro mol rest ...))]))

(define-syntax sync-macro
  (syntax-rules ()
    [(_m )  (void)]
    [(_ m (path paths ...) rest ...)
     (begin (sync-paths m 'path 'paths ...)
            (sync-macro m rest ...))]))

(def (height mol)
  (match (send mol get-kids)
    [(list)  0]
    [kids    (add1 (apply max (map height kids)))]))

(def (complexity mol)
  (add1 (sum-list (map complexity
                       (send mol get-kids)))))

(def (ref mol path)
  ;; Reference a descendant.
  (if (null? path)
      mol
      (match (send mol refr (car path))
        ['NOT-FOUND 'NOT-FOUND]
        [kid        (ref kid (cdr path))])))

(def (set-type-path mol path val)
  (send mol expand path no-fail)
  (send (ref mol path) set-type val))

(def (ref-data mol path)
  (match (ref mol path)
    ['NOT-FOUND '?DATA]
    [kid        (send kid get-data)]))

(def (ref-type mol path)
  (match (ref mol path)
    ['NOT-FOUND '?TYPE]
    [kid        (send kid get-type)]))

(def (update-path mol path val
                  [fail-con no-fail])
  (match (let/ec escape
           (match (ref mol path)
             ['NOT-FOUND (send mol expand
                           path
                           (thunk (escape 'FAIL)))
                         (send (ref mol path) update
                           val no-fail)]
             [kid        (send kid update
                           val
                           (thunk (escape 'FAIL)))]))
    ['FAIL  (fail-con)]
    [_      (void)]))

(def (expand-and-get-paths mol paths)
  (for      ([p paths]) (send mol expand p))
  (for/list ([p paths]) (ref mol p)))

(def (sync-paths mol
                 paths
                 [fail-con no-fail])
  (check-false (null? paths)
               "sync-paths must be given than one paths")

  (match (let/ec escape
           (let* ([mols    (expand-and-get-paths mol paths)]
                  [master   (first mols)]
                  [servants (list-tail mols 1)])
             (for ([m servants])
               (send m sync
                 master
                 (thunk (escape 'FAIL))))))
    ['FAIL  (fail-con)]
    [_      (void)]))

(def (preserve mol paths)
  (for ([m  (expand-and-get-paths mol paths)])
    (send m mark-no-touch)))

(def (distinguish mol paths)
  ;; Do not let these molcules sync with each other.
  ;; note: overwrites existing no-sync policies
  ;; note: error checking is left to the user
  (let ([mols (expand-and-get-paths mol paths)])
    (for ([m mols])
      (send m set-no-sync
        (remq m mols)))))

(def (mark-no-touch-paths mol paths)
  (for ([m (expand-and-get-paths mol paths)])
    (send m mark-no-touch)))
