#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt")

(provide (all-defined-out))

(def (instance ins model)
  ;; Check if `ins` is an instance of `model`
  (def (ctor-arity s)
    (case s
      [(->)  2]))

  (def (same m1 m2)
    ;; the meaning of the saying "m1 is synced with m2"
    (or (eq? m1 m2)
       (let ([ctor  (mol%-data m1)])
         (match (mol%-data m2)
           ['no-dat  #f]
           [ctor     (let ([kids1  (mol%-kids m1)]
                           [kids2  (mol%-kids m2)])
                       (and (eq*? (ctor-arity ctor)
                                (length kids1)
                                (length kids2))
                          (for/and ([kid1  kids1]
                                    [kid2  kids2])
                            (same kid1 kid2))))]
           [_        #f]))))

  (begin (set! ins   (copy ins))
         (set! model (copy model)))

  (let/ec escape
    (cascade-path model
                  (lam (mol path)
                    (match (mol%-data mol)
                      ['no-dat  (void)]
                      [md       (match (ref-data ins path)
                                  [md  (void)]
                                  [_   (escape #f)])]))
                  #|Data requirement|#)

    (let ([topo  (topology model)])
      (for/and ([chain  topo])
        (for ([path  chain])
          (update! ins path)
          #|Equip ins in case it is simpler|#)

        (let ([mcentral  (ref ins (car chain))])
          (for/and ([path  (cdr chain)])
            (same (ref ins path)
                  mcentral))))
      #|topology requirement|#)))

(def (complexity m)
  (add1 (sum-list (map complexity
                       (mol%-kids m)))))

(def (main database)
  (def (make-mp fun arg)
    (pull (pull mp fun '[1])
          arg '[2]))

  (def (conclusion root)
    (copy root '[0]))

  (def (log-discard m1 m2)
    (let ([c1 (conclusion m1)]
          [c2 (conclusion m2)])
      (unless (equal? c1 c2)
        (newline) (dm c1)
        (displayln "Replaced by")
        (dm c2) (newline)))

    (display "x"))

  (let ([mixed  (shuffle database)]
        [i      -1])
    (let loop ([new-db null]
               [m1     (car mixed)]
               [mixed  (cdr mixed)])
      (set! i (add1 i))
      (match mixed
        [(list)          (cons m1 new-db)]
        [(cons m2 mrst)
         (match (< i 2)
           [#t  (match (make-mp m1 m2)
                  [#f  (loop (cons m1 new-db)
                             m2
                             mrst)  #|conflict|#]
                  [m3  (match (> (height (conclusion m3))
                                 20)
                         [#t  (loop (cons m1 new-db)
                                    m2
                                    mrst)  #|Gotta do w/o this one|#]
                         [#f  (loop (cons (pull (update (new-mol) '[] 'mp=>)
                                                (conclusion m3)
                                                '[0] #|We cannot store the entire proof |#)
                                          (cons m1 new-db))
                                    m2
                                    mrst)])])]

           [#f  (match (instance (conclusion m1)
                                 (conclusion m2))
                  [#t  (log-discard m1 m2)
                       (match (< (complexity m1)
                                 (complexity m2))
                         [#t  (match (instance m2 m1)
                                [#t  (loop new-db  m1  mrst)]
                                [#f  (loop new-db  m2  mrst)])]
                         [#f  (loop new-db  m2  mrst)])]
                  [#f  (match (instance m2 m1)
                         [#t  (log-discard m2 m1)
                              (loop new-db  m1  mrst)]
                         [#f  (loop (cons m1 new-db)
                                    m2
                                    mrst)])])])]))))
