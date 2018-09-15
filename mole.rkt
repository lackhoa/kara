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
  ;; Convenience method
  (match (ref root path)
    ['not-found  'no-dat]
    [mol         (mol-data mol)]))

(def (replace mol path new)
  (let loop ([path path]
             [m    mol])
    (match path
      [(list)               new]
      [(cons next-id rest)
       (mol-update-kids
        m
        (lam (kids)
          (list-set kids
                    next-id
                    (loop rest
                          (list-ref kids next-id)))))])))

(def (do-inform root mol proc)
  ;; returns the root with `proc` done to `mol` and its associates.
  ;; proc: mol -> mol | 'conflict
  (let loop ([sync-ls (mol-sync mol)]
             [root    root])
    (match sync-ls
      [(list)                 root]
      [(cons next-path rest)
       (match (proc (ref root next-path))
         ['conflict  'conflict]
         [new-mol    (let ([root (replace root
                                          next-path
                                          new-mol)])
                       (loop rest root))])])))

(def (update root
             [path null]
             [val  'no-dat])
  ;; Public interface
  (let loop ([root  root]
             [rel   null]
             [path  path])
    (let ([mol  (ref root rel)])
      (match path
        [(list)
         (match* ((mol-data mol) val)
           [(any any)    root]
           [('no-dat _)  (do-inform root
                                    mol
                                    (lam (m) (mol-set-data m val)))]
           [(_ _)        'conflict])]

        [(cons next-id rest-path)
         (let ([kids  (mol-kids mol)])
           (match (<= next-id
                     (last-index kids))
             [#t  (loop root
                        (pad rel next-id)
                        rest-path)]
             [#f  (let ([root (do-inform root
                                         mol
                                         (lam (m)
                                           (mol-set-kids m
                                                         (let ([fillers (for/list ([i (range (add1 (last-index kids))
                                                                                             (add1 next-id))])
                                                                          (make-mol (for/list ([p (mol-sync m)])
                                                                                      ;; Just the sync list
                                                                                      (pad p i))))])
                                                           (append kids fillers)))))])
                    (loop root
                          (pad rel next-id)
                          rest-path))]))]))))

(def (sync root p1 p2)
  (let* ([m1 (ref root p1)]
         [m2 (ref root p2)]
         [combined (append (mol-sync m1)
                           (mol-sync m2))])
    (let ([root (replace root
                         p1
                         (mol-set-sync m1 combined))])
      (replace root
               p2
               (mol-set-sync m2 combined)))))
