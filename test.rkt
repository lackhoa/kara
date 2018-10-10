#lang racket
(require "lang/kara.rkt")

(def (num->pair n list1 list2)
  (let-values ([(i1 i2)  (quotient/remainder n
                                             (length list2))])
    (cons (list-ref list1 i1)
          (list-ref list2 i2))))
