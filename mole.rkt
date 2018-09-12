#lang racket
(require "lang/kara.rkt"
         "types.rkt"
         racket/hash)
(provide (all-defined-out))

(def (no-fail)
  ;; The FAIL continuation that you're
  ;; confident not gonna be called.
  (error "I did not expect this to fail!"))


(struct mole% (data      ; '?DATA | symbols
               dic       ; hashtable of role-kid
               sync-ls   ; a list of molecules

               type      ; '?TYPE | type (stream of model molecules)
               no-sync   ; a list of molecules
               expanded? ; whether or not this is well-formed
               no-touch? ; whether or not this can be updated
               ))

(def (make-mole)
  (mole% '?DATA (hasheq) null
         '?TYPE null #f #f))

(def (get-roles mole)
  (hash-keys (mole%-dic mole)))

(def (get-kids mole)
  (hash-values (mole%-dic mole)))

(def (set-data mole new-data fail-con)
  (match (mole%-no-touch? mole)
    [#t  (fail-con)]
    [#f  (struct-copy mole% mole
                      [data new-data])]))

(def (set-type mole new-type fail-con)
  (struct-copy mole% mole
               [type new-type]))

(def (set-sync-ls mole new-sync-ls fail-con)
  (check-pred list? new-sync-ls
              "Sync list must be non-empty")

  (cond [(set-empty? (set-intersect no-sync
                                    new-sync-ls))
         (struct-copy mole% mole
                      [sync-ls new-sync-ls])]
        [else  (fail-con)]))

(def (mark-expanded mole)
  (struct-copy mole% mole
               [expanded? #t]))

(def (mark-expanded mole)
  (struct-copy mole% mole
               [expanded? #t]))

(def (add-kid mole role kid fail-con)
  (check-pred number? role
              "Role is a number")
  (check-class mole mole%
               "Kid is a molecule")
  (cond [no-touch?  (fail-con)]
        [else
         (struct-copy mole% mole
                      [dic (hash-set (mole%-dic mole)
                                     role
                                     kid)])]))

(def (set-no-sync mole new-no-sync)
  ;; This method is intended to be used
  ;; for setting up new molecules as proof goals,
  (let ([sync-ls (mole%-sync-ls mole)])
    (cond [(set-empty? (set-intersect new-no-sync sync-ls))
           (struct-copy mole% mole
                        [no-sync new-no-sync])]
          [else
           (error "SET-NO-SYNC -- Illegal state" sync-ls ls)])))

(def (classify)
  ;; For representation purpose only
  (match* ((get-kids mole) data)
    [((list) '?DATA)  'BLANK]
    [((list) _)       'ATOM]
    [(_ _)            'STRUCT]))

(define/public (repr)
  (repr-core (make-repr-env (make-hasheq)
                            (let ([count 64])
                              ;; The counting closure
                              (thunk (set! count (add1 count))
                                     (integer->char count))))))

(def (make-repr-env mole env get-next)
  ;; Sort out which variables are
  ;; represented by which symbol.
  (def (hash-set-many ht ls val)
    (let loop ([ls ls] [ht ht])
      (match ls
        [(list)  ht]
        [(cons item rest)
         (loop rest (hash-set ht item val))])))

  (match (classify)
    ['BLANK   (match (hash-ref env this 'UNBOUND)
                ['UNBOUND (hash-set-many env sync-ls (get-next))]
                [_
                 ;; Already bound
                 env])]
    ['ATOM    env]
    ['STRUCT  (let loop ([kids  (get-kids mole)]
                         [env   env])
                (match kids
                  [(list) env]
                  [(cons kid rest)
                   (loop rest (make-repr-env kid env get-next))]))]))

(define/public (repr-core mole env)
  (match (classify mole)
    ['BLANK   (hash-ref env mole)]
    ['ATOM    (mole%-data mole)]
    ['STRUCT  (cons (match (mole%-data mole) ['?DATA '?] [any any])
                    (for/list ([role
                                (range (add1 (apply max (get-roles))) #|The ceiling|#)])
                      (match (refr mole role)
                        ['NOT-FOUND  '-]
                        [kid         (repr-core kid env)])))]))

(define/public (refr mole role)
  ;; Like `ref`, but reference a direct kid
  (hash-ref (mole%-dic mole)  role  'NOT-FOUND))

;; (def (inform task)
;;   ;; Tell those in the sync list to do something.
;;   ;; `task` is a function takes a molecule.
;;   (check-pred procedure? task)
;;   (for ([subject (remq this sync-ls)])
;;     (task subject)))

(def mole%
  (class* object% (writable<%>)


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

    (define/public (just-expand path fail-con)
      ;; Keep adding new kids
      ;; until the path is exhausted.
      (unless (null? path)
        (match (let/ec escape
                 (let ([new-kid (new mole%)])
                   (add-kid (car path)
                            new-kid
                            (thunk (escape 'FAIL)))
                   (send new-kid just-expand
                     (cdr path)
                     (thunk (escape 'FAIL)))))
          ['FAIL  (fail-con)]
          [_      (void)])))

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
                     (thunk (escape 'FAIL-LOW))))

                 'OK)

          ['OK  (void)]
          [_    (fail-con)])))))


;;; Macros
(define-syntax update-macro
  (syntax-rules ()
    [(_ mole)  (void)]

    [(_ mole (path ctor) rest ...)
     (begin
       (send mole update-path
         'path ctor)
       (update-macro mole rest ...))]))

(define-syntax sync-macro
  (syntax-rules ()
    [(_m )  (void)]
    [(_ m (path paths ...) rest ...)
     (begin (sync-paths m 'path 'paths ...)
            (sync-macro m rest ...))]))

(def (height mole)
  (match (send mole get-kids)
    [(list)  0]
    [kids    (add1 (apply max (map height kids)))]))

(def (complexity mole)
  (add1 (sum-list (map complexity
                       (send mole get-kids)))))

(def (ref mole path)
  ;; Reference a descendant.
  (if (null? path)
      mole
      (match (send mole refr (car path))
        ['NOT-FOUND 'NOT-FOUND]
        [kid        (ref kid (cdr path))])))

(def (set-type-path mole path val)
  (send mole expand path no-fail)
  (send (ref mole path) set-type val))

(def (ref-data mole path)
  (match (ref mole path)
    ['NOT-FOUND '?DATA]
    [kid        (send kid get-data)]))

(def (ref-type mole path)
  (match (ref mole path)
    ['NOT-FOUND '?TYPE]
    [kid        (send kid get-type)]))

(def (update-path mole path val
                  [fail-con no-fail])
  (match (let/ec escape
           (match (ref mole path)
             ['NOT-FOUND (send mole expand
                           path
                           (thunk (escape 'FAIL)))
                         (send (ref mole path) update
                           val no-fail)]
             [kid        (send kid update
                           val
                           (thunk (escape 'FAIL)))]))
    ['FAIL  (fail-con)]
    [_      (void)]))

(def (expand-and-get-paths mole paths)
  (for      ([p paths]) (send mole expand p))
  (for/list ([p paths]) (ref mole p)))

(def (sync-paths mole
                 paths
                 [fail-con no-fail])
  (check-false (null? paths)
               "sync-paths must be given than one paths")

  (match (let/ec escape
           (let* ([moles    (expand-and-get-paths mole paths)]
                  [master   (first moles)]
                  [servants (list-tail moles 1)])
             (for ([m servants])
               (send m sync
                 master
                 (thunk (escape 'FAIL))))))
    ['FAIL  (fail-con)]
    [_      (void)]))

(def (preserve mole paths)
  (for ([m  (expand-and-get-paths mole paths)])
    (send m mark-no-touch)))

(def (distinguish mole paths)
  ;; Do not let these molecules sync with each other.
  ;; note: overwrites existing no-sync policies
  ;; note: error checking is left to the user
  (let ([moles (expand-and-get-paths mole paths)])
    (for ([m moles])
      (send m set-no-sync
        (remq m moles)))))

(def (mark-no-touch-paths mole paths)
  (for ([m (expand-and-get-paths mole paths)])
    (send m mark-no-touch)))
