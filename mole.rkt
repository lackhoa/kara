#lang racket
(require "lang/kara.rkt"
         racket/hash)
(provide (all-defined-out))

(def (no-fail)
  ;; The FAIL continuation that you're
  ;; confident not gonna be called.
  (error "I did not expect this to fail!"))

;;; Molcules
(def (make-mol sync-ls
               [data    'no-dat]
               [kids    null])
  ;; data    : 'no-dat  | symbol
  ;; kids    : 'no-kid  | mols
  ;; sync-ls : mols (at least one path)
  (check-true (> (set-count sync-ls) 0))
  (list sync-ls data kids))

(def (make-root)
  (make-mol '([])))

;;; Getters for molcule
(def mol-sync first)
(def mol-data second)
(def mol-kids third)

;;; Setters for molecule
(def (mol-set-sync mol val)
  (list-set mol 0 val))
(def (mol-set-data mol val)
  (list-set mol 1 val))
(def (mol-set-kids mol val)
  (list-set mol 2 val))

;;; Updaters for molecule
(def (mol-update-sync mol proc)
  (mol-set-sync mol
                (proc (mol-sync mol))))
(def (mol-update-kids mol proc)
  (mol-set-kids mol
                (proc (mol-kids mol))))

;; Other methods for molecules
(def (mol-repr root mol)
  ;; Public interface
  (def (blank? mol)
    (match* ((mol-data mol) (mol-kids mol))
      [('no-dat (list))  #t]
      [(_       _)       #f]))

  (def (make-repr-env mol
                      in-env
                      get-next!)
    ;; Sort out which variables are
    ;; represented by which symbol.
    (def (hash-set-many ht ls val)
      (let loop ([ls ls] [ht ht])
        (match ls
          [(list)            ht]
          [(cons item rest)
           (loop rest (hash-set ht item val))])))

    (match (blank? mol)
      [#t  (match (hash-ref in-env mol 'unbound)
             ['unbound  (hash-set-many in-env
                                       (for/list ([p (mol-sync mol)])
                                         (ref root p))
                                       (get-next!))]
             [_         in-env])]
      [#f  (let loop ([kids  (mol-kids mol)]
                      [env   in-env])
             (match kids
               [(list)           env]
               [(cons kid rest)
                (loop rest (make-repr-env kid env get-next!))]))]))

  (def (mol-repr-core mol env)
    (match (blank? mol)
      [#t  (hash-ref env mol)]
      [#f  (cons (match (mol-data mol)
                   ['no-dat  '?]
                   [any      any])
                 (for/list ([k (mol-kids mol)])
                   (mol-repr-core k env)))]))

  (mol-repr-core mol (make-repr-env mol
                                    (hasheq)
                                    (let ([count 64])
                                      ;; The counting closure
                                      (thunk (set! count (add1 count))
                                             (integer->char count))))))

(def (ref root path)
  (match path
    [(list)            root]
    [(cons next rest)
     (let ([kids  (mol-kids root)])
       (match (<= next (sub1 (length kids)))
         [#t  (ref (list-ref kids next)
                   rest)]
         [#f  'not-found]))]))

(def (ref-data root path)
  ;; Programmer convenience method
  (match (ref root path)
    ['not-found  'no-dat]
    [mol         (mol-data mol)]))

(def (ref-kids root path)
  ;; Programmer convenience method
  (match (ref root path)
    ['not-found  null]
    [mol         (mol-kids mol)]))

(def (ref-sync root path)
  ;; Programmer convenience method
  (match (ref root path)
    ['not-found  (list path)]
    [mol         (mol-sync mol)]))

(def (do&inform root path proc)
  ;; returns the root with `proc` done to `ref path` and its associates.
  ;; proc: mol -> mol | 'conflict

  (def (replace mol path new)
    ;; Crucial auxiliary function
    (let loop ([path  path]
               [m     mol])
      (match path
        [(list)               new]
        [(cons next-id rest)
         (mol-update-kids m
                          (lam (kids)
                            (list-set kids
                                      next-id
                                      (loop rest (list-ref kids next-id)))))])))

  (let loop ([sync-ls  (ref-sync root path)])
    (match sync-ls
      [(list)                 root]
      [(cons next-path rest)
       (match (proc (ref root next-path))
         ['conflict  'conflict]
         [new-mol    (set! root (replace root
                                         next-path
                                         new-mol))
                     (loop rest)])])))

(def (update root
             path
             [val  'no-dat])
  ;; Public data-updating/expanding interface
  ;; if val is 'no-dat, do not overwrite existing data
  (let loop ([rel  #|path so far|# null]
             [path #|path left|#   path])
    (match path
      [(list)  (match* ((ref-data root rel) val)
                 [(any any)    root]
                 [('no-dat _)  (do&inform root
                                          rel
                                          (lam (m) (mol-set-data m val)))]
                 [(_ 'no-dat)  root]
                 [(_ _)        'conflict])]

      [(cons next-id rest-path)
       (let ([kids  (ref-kids root rel)])
         (when (<= (last-index kids)
                  next-id)
           (set! root
                 (do&inform root
                            rel
                            (lam (m)
                              (mol-set-kids m
                                            (let ([fillers (for/list ([i (range (add1 (last-index kids))
                                                                                (add1 next-id))])
                                                             (make-mol (for/list ([p (mol-sync m)])
                                                                         ;; Just the sync list
                                                                         (pad p i))))])
                                              (append kids fillers))))))))
       (loop (pad rel next-id)
             rest-path)])))



(def (kids-indices root path)
  ;; Programmer's utility
  (range (length (ref-kids root path))))

(def (sync root p1 p2)
  ;; Establish a new synchronization, expand if needed.

  (def (height mol)
    (match (mol-kids mol)
      [(list)  0]
      [kids    (add1 (apply max (map height kids)))]))

  (begin (set! root (update root p1))
         (set! root (update root p2))
         #|Make sure the paths exist|#)

  (match (member p1 (ref-sync root p2))
    [(not #f)  root]
    [#f        (let/ec escape
                 (match* ((ref-data root p1)
                          (ref-data root p2))
                   [(x x)  (void)]

                   [('no-dat other-dat)
                    (set! root (update root p1 other-dat))]

                   [(dat 'no-dat)
                    (set! root (update root p2 dat))]

                   [(_ _)  (escape 'conflict)])

                 (let* ([mh  (lam (root)
                               (max (height (ref root p1))
                                    (height (ref root p2))))]
                        [oh  (mh root)  #|monitor the old height|#]
                        [i1  (last-index (ref-kids root p1))]
                        [i2  (last-index (ref-kids root p2))])
                   ;; Add the missing kids
                   (cond [(= i1 i2)  root]
                         [(< i1 i2)  (set! root (update root
                                                        (pad p1 i2)))]
                         [(> i1 i2)  (set! root (update root
                                                        (pad p2 i1)))])
                   (when (> (mh root) oh)
                     ;; The max height cannot increase, or it's a cycle
                     (escape 'conflict)))

                 (let ([combined (append (ref-sync root p1)
                                         (ref-sync root p2))])
                   ;; Establish the connection
                   (set! root
                         (do&inform root
                                    p1
                                    (lam (m)
                                      (mol-set-sync m combined))))
                   (set! root
                         (do&inform root
                                    p2
                                    (lam (m)
                                      (mol-set-sync m combined)))))

                 (for ([i  (kids-indices root p1)])
                   ;; Our job is over, let the kids sync
                   (match (sync root
                                (pad p1 i)
                                (pad p2 i))
                     ['conflict  (escape 'conflict)]
                     [new-root   (set! root new-root)]))

                 root)]))

(def (copy root path new-path)
  ;; Public interface

  (def (swap-prefix ls)
    (for/list ([li  (filter (lam (x) (list-prefix? path x))
                            ls   #|Weed out the non-descendants|#)])
      (append new-path
              (list-tail li (length path)))))

  (let loop ([rel  null])
    (let ([focus      (append path rel)]
          [focus-new  (append new-path rel)])
      (set! root
            (replace root
                     focus-new
                     (make-mol (swap-prefix (ref-sync root focus)
                                            #|copied sync list|#)
                               (ref-data root focus))))
      (for ([i  (kids-indices root focus)])
        (set! root  (loop (pad rel i)))))
    root))

(def (complexity mol)
  ;; Crucial theoretical number.
  (+ (if (eq? (mol-data mol) 'no-dat)
         0
         1)
     (sum-list (map complexity
                    (mol-kids mol)))))
