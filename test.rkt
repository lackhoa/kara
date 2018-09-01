#lang racket
(require "lang/kara.rkt")

(define-syntax-rule (fluid-let ((x e))
                      b1 b2 ...)
  (let* ([y e]
         [swap
          (thunk (let ([t x])
                   (set! x y)
                   (set! y t)))])
    (dynamic-wind
      swap
      (thunk b1 b2 ...)
      swap)))

(def reenter #f)
(def x 0)
(fluid-let ([x 1])
  (call/cc (lam (k) (set! reenter k)))
  (set! x (+ x 1))
  x)
x
(reenter '*)
(reenter '*)
x
