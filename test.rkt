#lang racket
(provide place-main)

(define (f i)
  (match i
    [0  0]
    [_  (+ i (f (sub1 i)))]))

(define (f2 n)
  (for/fold ([res  0]) ([i (in-range n)])
    (+ res i)))
