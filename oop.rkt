#lang racket
(require "lang/kara.rkt")

(def fish%
  (class* object% (writable<%>)
    (init size)

    (define/public (custom-display port)
      (display "I'm a fish" port))

    (define/public (custom-write port)
      (display "I'm a fish" port))

    (define current-size size)

    ; This won't work as you intended
    (define double-size
      (* 2 current-size))

    (define/public (get-double-size)
      double-size)

    (super-new)

    (define/public (get-size)
      current-size)

    (define/public (grow amt)
      (set! current-size (+ amt current-size)))

    (define/public (eat other-fish)
      (grow (send other-fish get-size)))))

(def f (new fish% [size 5]))
(displayln f)
(send f grow 6)
(send f get-size)
(send f get-double-size)
