#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "types.rkt"
         future-visualizer)

(provide (all-defined-out))

(def (instance? ins model)
  ;; Check if `ins` is an instance? of `model`
  (def (ctor-arity s)
    (case s
      [(->)   2]
      [else  0  #|Enable the use of arbitrary variables|#]))

  (def (same? root path1 path2)
    ;; the meaning of uttering "path1 is synced with path2"
    (let* ([root  (update root path1)]
           [root  (update root path2)]
           [mol1  (ref root path1)]
           [mol2  (ref root path2)])
      (orb (member path1 (mol%-sync mol2))
           (let ([ctor  (mol%-data mol1)])
             (match (mol%-data mol2)
               [#f        #f]
               [(== ctor)  (let ([kpaths1  (kids-paths root path1)]
                                [kpaths2  (kids-paths root path2)])
                            (andb (eq? (ctor-arity ctor)
                                       (length kpaths1)
                                       (length kpaths2))
                                  (for/andb ([kp1  kpaths1]
                                             [kp2  kpaths2])
                                    (same? root kp1 kp2))))]
               [_         #f])))))

  (let/ec escape
    (let loop ([path  '[]])
      (let ([recur  (thunk
                     (for ([kid-path  (kids-paths model path)])
                       (loop kid-path)))])
        (match (ref-data model path)
          [#f  (recur)]
          [md  (match (ref-data ins path)
                 [md  (recur)]
                 [_   (escape #f)])]))  #|Data requirement|#)

    (let loop ([path  '[]])
      (let* ([sync-ls   (ref-sync model path)]
             [pcentral  (car sync-ls)])
        (andb (for/andb ([p  (cdr sync-ls)])
                (same? ins pcentral p))
              (for/andb ([kid-path  (kids-paths model path)])
                (loop kid-path))))  #|Topology requirement|#)))

(def (complexity m)
  (add1 (sum-list (map complexity
                       (mol%-kids m)))))

(def (conclusion root)
  (detach root '[0]))

(def (collide database)
  (def (core-collide database)
    ;; Work on a single core
    (def (log-discard c1 c2)
      (display "-"))

    (let ([pool  database])
      (let loop ([new-db null]
                 [cm1    (car pool)]
                 [m1     (decompress (car pool))]
                 [pool   (cdr pool)])
        (match pool
          [(list)           (cons cm1 new-db)]
          [(cons cm2 mrst)  (let* ([m2  (decompress cm2)]
                                   [c1  (conclusion m1)]
                                   [c2  (conclusion m2)])
                              (match (instance? c1 c2)
                                [#t  (match (< (complexity c1)
                                               (complexity c2))
                                       [#t  (match (instance? c2 c1)
                                              [#t  (log-discard c2 c1)
                                                   (loop new-db  cm1  m1  mrst)]
                                              [#f  (log-discard c1 c2)
                                                   (loop new-db  cm2  m2  mrst)])]
                                       [#f  (log-discard c1 c2)
                                            (loop new-db  cm2  m2  mrst)])]
                                [#f  (match (instance? c2 c1)
                                       [#t   (log-discard c2 c1)
                                             (loop new-db  cm1  m1  mrst)]
                                       [#f  (loop (cons cm1 new-db)
                                                  cm2
                                                  m2
                                                  mrst)])]))]))))

  (let-values ([(db1 db2)  (split-at (shuffle database)
                                     (floor (/ (length database)
                                               2)))])
    (visualize-futures
     (let ([f  (future (thunk (core-collide db2)))])
       (let ([now  (core-collide db1)])
         (append now (touch f)))))))

(def (combine database)
  (def (make-mp fun arg)
    (>> (pull mp '[1] fun)
        (lam (mol)
          (pull mol '[2] arg))))

  (let* ([len  (length database)]
         [fst  (decompress (list-ref database
                                     (random len)))]
         [snd  (decompress (list-ref database
                                     (random len)))])
    (match (make-mp fst snd)
      [#f   database]
      [new  (let ([cn  (conclusion new)])
              (match (> (height cn) 30)
                [#t  database  #|Gotta do w/o this one|#]
                [#f  (display "+")
                     (cons (cmol% 'mp=> `(,(compress cn))  #|Keep only the conclusion|#)
                           database)]))])))
