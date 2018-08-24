#lang racket
(require "macro.rkt"
         rackunit
         racket/generator)
(provide (all-defined-out))

; -----------------------------------------------------------
; Functional Stuff
; -----------------------------------------------------------
(def (repeat func times)
    (when (> times 0)
        (func)
        (repeat func (- times 1))))

(def (nequal? x y)
  (not (equal? x y)))

(def (neq? x y)
  (not (eq? x y)))

; ------------------------------------------------------------
; Testing Functions
; ------------------------------------------------------------
(def (fib n)
    (cond [(= 0 n) 1]
          [(= 1 n) 1]
          [else (+ (fib (- n 1)) (fib (- n 2)))]))

(def (square n)
    (* n n))

; ------------------------------------------------------------
; Assertions
; ------------------------------------------------------------
(define-simple-check (check-class obj class)
  (is-a? obj class))

; ------------------------------------------------------------
; Generators
; ------------------------------------------------------------
(def (gen-get gen
              [num 10]
              [func (lam (x)
                      (parameterize ([pretty-print-columns 50])
                        (pretty-display x))
                      (newline))])
  (cond [(not (number? num))
         (raise "Expected a number")]

        [(= num 0) (void)]

        [(> num 0)
         (match (gen)
           ['DONE (void)]
           [val (func val)
                (gen-get gen
                         (- num 1)
                         func)])]

        [else (raise "Invalid number")]))

; The impersonator pattern: yield all
; values that `gen` yields.
(define-syntax-rule (gen-impersonate gen)
  ; `g` stops the generator from being re-defined every loop
  (let ([g gen])
    (let loop ()
      (match (g)
        ['DONE 'DONE]
        [any
         (begin (yield any)
                (loop))]))))
