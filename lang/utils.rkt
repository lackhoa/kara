#lang racket
(require "macro.rkt"
         rackunit)
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
