#lang racket
(require "macro.rkt")

(provide stack%)

(define stack%
  (class object%
    (define content null)
    (super-new)

    (define/public (push item)
      (set! content (cons item content)))

    (define/public (pop)
      (when (null? content)
        (error "pop!" "Cannot pop an empty stack!" this))
      (let ([popped-item (car content)])
        (set! content (cdr content))
        popped-item))

    (define/public (peek)
      (car content))))
