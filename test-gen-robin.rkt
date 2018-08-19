#lang racket
(require "lang/kara.rkt"
         "engine.rkt"
         "gen-robin.rkt")

;; Testingu!
(def (fib n)
  (check-timer)
  (if (< n 2)
      n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(def fib-gen1
  (generator ()
    (let loop ([n 4])
      (yield (fib n))
      (if (<= n 11)
          (loop (+ n 1))
        'DONE))))

(def fib-gen2
  (generator ()
    (let loop ([n 10])
      (yield (fib n))
      (if (>= n 4)
          (loop (- n 1))
        'DONE))))

(def g (gen-robin (list fib-gen2 fib-gen1)))
(gen-get g 15)
