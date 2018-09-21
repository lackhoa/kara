#lang racket
(require "lang/kara.rkt")

(struct fish (x y) #:prefab)

(def f (fish 12 34))

(def g (fish f f))

(define gg
  (let-values ([(in out) (make-pipe)])
    (parameterize ([print-graph #t])
      (write g out))
    (read in)))

(check-eq? (fish-x gg) (fish-y gg)
           "Even though identity is still preserved")
(check-false (eq? (fish-x gg) f)
             "The object has been cloned")
