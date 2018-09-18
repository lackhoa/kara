#lang racket
(require "lang/kara.rkt"
         racket/hash)
(provide mol-repr update sync new-root
         pull attach detach ref-data
         ref-sync ref-kids kids-paths)

(def (no-fail)
  ;; The FAIL continuation that you're
  ;; confident not gonna be called.
  (error "I did not expect this to fail!"))

;;; Molcules
(struct mol% (sync data kids) #:prefab)

(def (make-mol sync-ls
               [data    'no-dat]
               [kids    null])
  ;; data    : 'no-dat  | symbol
  ;; kids    : 'no-kid  | mols
  ;; sync-ls : mols (at least one path)
  (check-true (> (set-count sync-ls) 0))
  (mol% sync-ls data kids))

(def (new-root)
  (make-mol '([])))

;; Other methods for molecules
(def (mol-repr root [path null])
  (def (blank? mol)
    (match mol
      [(mol% _ 'no-dat (list))  #t]
      [_                        #f]))

  (def (make-repr-env mol
                      in-env
                      get-next!)
    ;; Sort out which variables are
    ;; represented by which symbol.
    (match (blank? mol)
      [#t  (match (hash-ref in-env mol 'unbound)
             ['unbound  (hash-set-many in-env
                                       (map (lam (p)  (ref root p))
                                            (mol%-sync mol))
                                       (get-next!))]
             [_         in-env])]
      [#f  (let ([kids  (mol%-kids mol)]
                 [env   in-env])
             (for ([kid  kids])
               (set! env (make-repr-env kid
                                        env
                                        get-next!)))
             env)]))

  (def (mol-repr-core mol env)
    (match (blank? mol)
      [#t  (hash-ref env mol)]
      [#f  (cons (match (mol%-data mol)
                   ['no-dat  '?]
                   [any      any])
                 (map (lam (kid)  (mol-repr-core kid env))
                      (mol%-kids mol)))]))

  (mol-repr-core (ref root path)
                 (make-repr-env (ref root path)
                                (hash)
                                (let ([count 64])
                                  ;; The counting closure
                                  (thunk (set! count (add1 count))
                                         (integer->char count))))))

(def (ref root path)
  (match path
    [(list)            root]
    [(cons next rest)  (let ([kids  (mol%-kids root)])
                         (match (<= next (last-index kids))
                           [#t  (ref (list-ref kids next)
                                     rest)]
                           [#f  'not-found]))]))

(def (ref-data root path)
  (match (ref root path)
    ['not-found      'no-dat]
    [(mol% _ dat _)  dat]))

(def (ref-kids root path)
  (match (ref root path)
    ['not-found       null]
    [(mol% _ _ kids)  kids]))

(def (ref-sync root path)
  (match (ref root path)
    ['not-found        (list path)]
    [(mol% sync _ _ )  sync]))

(def (replace mol path new)
  ;; Crucial auxiliary function
  (let loop ([path  path]
             [m     mol])
    (match path
      [(list)               new]
      [(cons next-id rest)
       (struct-copy mol% m
                    [kids  (list-set (mol%-kids m)
                                     next-id
                                     (loop rest (list-ref (mol%-kids m)
                                                          next-id)))])])))

(def (do&inform root path proc)
  ;; returns the root with `proc` done to `ref path` and its associates.
  ;; proc: mol -> mol | 'conflict
  (let/ec escape
    (for ([p  (ref-sync root path)])
      (match (proc (ref root p))
        ['conflict  (escape 'conflict)]
        [new-mol    (set! root (replace root
                                        p
                                        new-mol))]))
    root))

(def (update root
             path
             [val  'no-dat])
  ;; Used to update or expand
  ;; if val is 'no-dat, do not overwrite existing data
  (let loop ([rel  #|path so far|# null]
             [path #|path left|#   path])
    (match path
      [(list)  (match* ((ref-data root rel) val)
                 [(any any)    root]
                 [('no-dat _)  (do&inform root
                                          rel
                                          (lam (m)  (struct-copy mol% m [data val])))]
                 [(_ 'no-dat)  root]
                 [(_ _)        'conflict])]

      [(cons next-id rest-path)
       (let ([kids  (ref-kids root rel)])
         (when (< (last-index kids)
                  next-id)
           (set! root
             (do&inform
              root
              rel
              (lam (m)
                (struct-copy mol% m
                             [kids (let* ([missing-indices  (range (add1 (last-index kids))
                                                                   (add1 next-id))]
                                          [fillers  (for/list ([i missing-indices])
                                                      (make-mol (for/list ([p (mol%-sync m)])
                                                                  (pad p i))
                                                                #|empty mole with inherited sync list|#))])
                                     (append kids fillers))]))))))
       (loop (pad rel next-id)
             rest-path)])))

(def (kids-paths root path)
  ;; Very helpful utility.
  (let ([kids-indices  (range (length (ref-kids root path)))])
    (for/list ([i  kids-indices])
      (pad path i))))

(def (height mol)
  ;; Used for synchronization
  (match (mol%-kids mol)
    [(list)  0]
    [kids    (add1 (apply max (map height kids)))]))

(def (sync root p1 p2)
  ;; Establish a new synchronization, expand if needed.
  (begin
    #|Make sure the paths exist|#
    (set! root (update root p1))
    (set! root (update root p2)))

  (let loop ([fp1  p1] [fp2  p2])
    (let/ec escape
      (when (member fp1 (ref-sync root fp2))
        #|Save ourselves some time here|#
        (escape root))

      (match* ((ref-data root fp1)
               (ref-data root fp2))
        [(x x)                (void)]
        [('no-dat other-dat)  (set! root  (update root fp1 other-dat))]
        [(dat 'no-dat)        (set! root  (update root fp2 dat))]
        [(_ _)                (escape 'conflict)])

      (let* ([mh  (max (height (ref root fp1))
                       (height (ref root fp2)))
                  #|watch the height to detect cycle|#]
             [i1  (last-index (ref-kids root fp1))]
             [i2  (last-index (ref-kids root fp2))])
        ;; Add the missing kids
        (cond [(< i1 i2)
               (set! root (update root
                                  (pad fp1 i2)))]

              [(> i1 i2)
               (set! root (update root
                                  (pad fp2 i1)))])

        (when (> (max (height (ref root fp1))
                      (height (ref root fp2)))
                 mh)
          (escape 'conflict)))

      (let ([combined  (remove-duplicates
                        (append (ref-sync root fp1)
                                (ref-sync root fp2)))])
        #|Establish the connections|#
        (set! root
          (do&inform root fp1 (lam (m)
                                (struct-copy mol% m [sync combined]))))
        (set! root
          (do&inform root fp2 (lam (m)
                                (struct-copy mol% m [sync combined])))))

      (for ([new-fp1 (kids-paths root fp1)]
            [new-fp2 (kids-paths root fp2)])
        ;; Our job is over, let the kids sync
        (match (loop new-fp1 new-fp2)
          ['conflict  (escape 'conflict)]
          [new-root   (set! root new-root)]))
      root)))

(def (migrate root from to)
  ;; Migrate the molecule from root-`from` to ?-`to`
  ;; Enables `attach` and `detach`
  (def (swap-prefix ls pre new-pre)
    (for/list ([li  (filter (lam (x) (list-prefix? pre x))
                            ls   #|Weed out the non-descendants|#)])
      (append new-pre
              (list-tail li (length pre)))))

  (let loop ([p  from])
    (mol% (swap-prefix (ref-sync root p)  from  to)
          (ref-data root p)
          (for/list ([kid-path  (kids-paths root p)])
            (loop kid-path)))))

(def (attach root branch to)
  (replace (update root to)
           to
           (migrate branch '[] to)))

(def (detach root from)
  ;; Convert root-path into a root
  (migrate root from '[]))

(def (pull host  guest  [to null])
  ;; It's like synchronizing, but with different roots
  (let* ([unifier (new-root)]
         [unifier (attach unifier host  '[0])]
         [unifier (attach unifier guest '[1])])
    (match (sync unifier
                 (append '[0] to)
                 '[1])
      ['conflict  'conflict]
      [unified    (detach unified '[0])])))
