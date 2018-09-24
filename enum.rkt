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

  (let/ec escape
    (cascade-path model
                  (lam (mol path)
                    (match (mol%-data mol)
                      ['no-dat  (void)]
                      [md       (match (ref-data ins path)
                                  [md  (void)]
                                  [_   (escape #f)])]))
                  #|Data check|#)

    (let ([topo  (topology model)])
      ;; topology mapping
      (for/and ([chain  topo])
        (match (ref ins (car chain))
          [#f        (escape #f)]
          [mcentral  (for/and ([path  (cdr chain)])
                       (same (ref ins path)
                             mcentral))])))))
