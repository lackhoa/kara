#lang racket
(require "lang/kara.rkt")

(def fish%
  (class* object% (writable<%>)
    (init size)                ; initialization argument

    (define/public (custom-display port)
      (display "I'm a fish" port))

    (define/public (custom-write port)
      (display "I'm a fish" port))

    (define current-size size) ; field

    (super-new)                ; superclass initialization

    (define/public (get-size)
      current-size)

    (define/public (grow amt)
      (set! current-size (+ amt current-size)))

    (define/public (eat other-fish)
      (grow (send other-fish get-size)))))

(def f (new fish% [size 5]))
(display f)
