#lang racket
(require "kara_macro.rkt")
(provide fib square tag tag-of content repeat)

; -----------------------------------------------------------
; Functional Stuff
; -----------------------------------------------------------
(def (repeat func times)
    (when (> times 0)
        (func)
        (repeat func (- times 1))))

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
