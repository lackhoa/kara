#lang racket
(require "lang/kara.rkt"
         racket/struct)

(define (odd-internal x)
  (define (even x)
    (if (zero? x)
        #t
        (odd-internal (sub1 x))))
  (if (zero? x)
      #f
      (even (sub1 x))))

(define (odd-external x)
  (if (zero? x)
      #f
      (even (sub1 x))))

(define (even x)
  (if (zero? x)
      #t
      (odd-external (sub1 x))))

(letrec ([odd (lambda (x)
                (if (zero? x)
                    #f
                    (even (sub1 x))))]
         [even (lambda (x)
                 (if (zero? x)
                     #t
                     (odd (sub1 x))))])
  (begin (display "Using letrec\n")
         (time (odd 40000000))))

(begin (display "Using internal definition\n")
       (time (odd-internal 40000000)))

(begin (display "Using external definition\n")
       (time (odd-external 40000000)))
