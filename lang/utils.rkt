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


; ------------------------------------------------------------
; Typing
; ------------------------------------------------------------
; An alias used to construct a term of a type.
(def (tag the-tag content)
    (list the-tag content))

; Don't reall need these two functions since we
; already have pattern-matching
(def (tag-of exp)
     (car exp))

(def (tagged? exp the-tag)
  (and (list? exp)
       (eq? (car exp) the-tag)))

(def (content tagged)
     (cadr tagged))

; ------------------------------------------------------------                           
; Testing Functions
; ------------------------------------------------------------                           
(def (fib n)
    (cond [(= 0 n) 1]
          [(= 1 n) 1]
          [else (+ (fib (- n 1)) (fib (- n 2)))]))

(def (square n)
    (* n n))
