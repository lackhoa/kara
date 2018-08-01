#lang racket
(require "macro.rkt"
         "stack.rkt"
         rackunit)

(def test-stack (new stack%))
(send test-stack push "First item")
(send test-stack push "Second item")
(check-equal? "Second item" (send test-stack peek))
(check-equal? "Second item" (send test-stack pop))
(check-equal? "First item" (send test-stack pop))
