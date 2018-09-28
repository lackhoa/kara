#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt")

(provide (all-defined-out))

(def (instance ins model)
  ;; Check if `ins` is an instance of `model`
  (def (ctor-arity s)
    (case s
      [(->)  2]
      [(?x ?y ?z ?w ?t ?u ?v ?A ?B ?C ?D)  0]))

  (def (same m1 m2)
    ;; the meaning of the saying "m1 is synced with m2"
    (or (eq? m1 m2)
       (let ([ctor  (mol%-data m1)])
         (match (mol%-data m2)
           ['no-dat   #f]
           [(== ctor)  (let ([kids1  (mol%-kids m1)]
                            [kids2  (mol%-kids m2)])
                        (and (eq*? (ctor-arity ctor)
                                 (length kids1)
                                 (length kids2))
                           (for/and ([kid1  kids1]
                                     [kid2  kids2])
                             (same kid1 kid2))))]
           [_         #f]))))

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

(def (conclusion root)
  (copy root '[0]))

(def (collide database)
  (def (log-discard c1 c2)
    ;; (unless (equal? c1 c2)
    ;;   (newline) (dm c1)
    ;;   (displayln "Replaced by")
    ;;   (dm c2))

    (display "-"))

  (let ([pool  (shuffle database)])
    (let loop ([new-db null]
               [m1     (car pool)]
               [pool   (cdr pool)])
      (match pool
        [(list)          (cons m1 new-db)]
        [(cons m2 mrst)  (let ([c1  (conclusion m1)]
                               [c2  (conclusion m2)])
                           (match (instance c1 c2)
                             [#t  (match (< (complexity c1)
                                            (complexity c2))
                                    [#t  (match (instance c2 c1)
                                           [#t  (log-discard c2 c1)
                                                (loop new-db  m1  mrst)]
                                           [#f  (log-discard c1 c2)
                                                (loop new-db  m2  mrst)])]
                                    [#f  (log-discard c1 c2)
                                         (loop new-db  m2  mrst)])]
                             [#f  (match (instance c2 c1)
                                    [#t   (log-discard c2 c1)
                                          (loop new-db  m1  mrst)]
                                    [#f  (loop (cons m1 new-db)
                                               m2
                                               mrst)])]))]))))

(def (combine database)
  (def (make-mp fun arg)
    (match (pull mp fun '[1])
      [#f   #f]
      [mol  (pull mol arg '[2])]))

  (let* ([len  (length database)]
         [fst  (list-ref database
                         (random len))]
         [snd  (list-ref database
                         (random len))])
    (match (make-mp fst snd)
      [#f   database  #|conflict|#]
      [new  (let ([cn  (conclusion new)])
              (match (> (height cn) 30)
                [#t  database  #|Gotta do w/o this one|#]
                [#f  (display "+")
                     (cons (mol% 'mp=> `(,cn)  #|the conclusion and the indicator that it was derived|#)
                           database)]))])))
