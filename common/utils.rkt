#lang racket
(require "kara.rkt")
(provide display-n display-nn stdisplay stdisplay-n stdisplay-nn
         fib square tag tag-of contents repeat)

; -----------------------------------------------------------
; I/O
; -----------------------------------------------------------
(def (display-n msg port)
    (display msg port) (newline port))
(def (display-nn msg port)
    (display msg port) (repeat (newline port) 2))

(def (stdisplay msg . obj) (display (apply format (cons msg obj)) (current-output-port)))
(def (stdisplay-n msg . obj) (display-n (apply format (cons msg obj)) (current-output-port)))
(def (stdisplay-nn msg . obj) (display-nn (apply format (cons msg obj)) (current-output-port)))

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
(def (tag the-tag contents)
    (list the-tag contents))

; Don't reall need these two functions since we
; already have pattern-matching
(def (tag-of exp)
     (car exp))

(def (contents tagged)
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
