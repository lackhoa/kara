#lang racket

(require rackunit
         "kara_macro.rkt")

(check-equal? (case '(z)
                [(tv i) "No!"]
                [(S n) (+ n 5)]
                [(z) "Yes!"])
             "Yes!"
             "Three patterns")

(check-equal? (case '(z)
                [(tv i) "No!"]
                [(S n) (+ n 5)]
                [(z) (define a 80) a])
             80
             "Compound body")


(check-equal? (case (list 'foo 5 6)
                [(z) "No!"]
                [(foo a b) (* a b)])
             30
             "Using two variables")

(def (max n m)
  (case n
    [(z)    m]
    [(S n0) (case m
              [(z)    (list 'S n0)]
              [(S m0) (list 'S (max n0 m0))])]))

(check-equal? (max (quote (S (z))) (quote (z)))
             (quote (S (z)))
             "Complex patterns 1")

(check-equal? (max (quote (S (S (S (z))))) (quote (S (S (S (S (z)))))))
             (quote (S (S (S (S (z))))))
             "Complex pattern 2")

(check-equal? ((const "Hello everyone"))  "Hello everyone" "Constant function")

"All test passed!"
