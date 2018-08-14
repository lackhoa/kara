#lang racket
(require "macro.rkt")
(provide (all-defined-out))

; -----------------------------------------------------------
; Functional Stuff
; -----------------------------------------------------------
(def (repeat func times)
    (when (> times 0)
        (func)
        (repeat func (- times 1))))

(def (and-pred . preds)
  (if (null? preds)
      #t
    (lam (x)
      (if ((car preds) x)
          ((apply and-pred (cdr preds)) x)
        #f))))

(def (or-pred . preds)
  (if (null? preds)
      #f
    (lam (x)
      (if ((car preds) x)
          #t
        ((apply or-pred (cdr preds)) x)))))

(def (unequal? x y)
  (not (equal? x y)))

(def (uneq? x y)
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
