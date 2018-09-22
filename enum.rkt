#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt")

(provide (all-defined-out))

(def (trim! root)
  ;; Constructor-based elevation of equality.
  (def mom-table
    (let ([mt (hasheq)])
      (let map! ([mol root])
        (for ([kid  (mol%-kids mol)])
          (set! mt (hash-set mt kid mol))
          (set! mt (hash-union mt (map! kid)))))
      mt))

  (def (get-mom mol)
    (hash-ref mom-table mol))

  (def (ctor-arity x)
    (case x
      [(->)  2]))

  (let do-trim! ([mol root])
    ;; This function modifies the immediate kids list
    (let ([kids  (mol%-kids mol)])
      (for ([i    (in-naturals)]
            [kid  kids])
        (let ([grandkids   (mol%-kids kid)]
              [common-mom  (get-mom (car grandkids))])
          (when (and (neq? common-mom  kid)
                   (neq? (mol%-data kid)
                         'no-dat)
                   (eq? (length grandkids)
                        (ctor-arity (mol%-data kid)))
                   (forall? (lam (gk)  (eq? (get-mom gk)
                                          common-mom))
                            (cdr grandkids)))
            (mol%-set-kids! mol
                            (list-set kids
                                      i
                                      common-mom))))
        (do-trim! kid)))))

(def (instance? mol1 mol2)
  (let/ec escape
    (let ([translator  (make-hasheq)])
      (let loop ([m1  mol1]
                 [m2  mol2])
        (match (hash-ref translator  m1  'not-found)
          ['not-found  (match (mol%-data m2)
                         [(== (mol%-data m1))  (hash-set! translator m1 m2)]
                         [_                   (escape #f)])]
          [val         (unless (eq? m2 val)
                         (escape #f))])
        (let ([kids1  (mol%-kids m1)]
              [kids2  (mol%-kids m2)])
          (match (<= (length kids1)
                    (length kids2))
            [#t  (for ([k1  kids1]
                       [k2  kids2])
                   (loop k1 k2))]
            [#f  (escape #f)]))))))
