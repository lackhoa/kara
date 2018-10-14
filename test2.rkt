#lang racket
(require "lang/kara.rkt"
         "test.rkt")

(let ([pls  (for/list ([i  (in-range 2)])
              (dynamic-place "test.rkt" 'main))])
  (place-channel-put (list-ref pls 0)
                     `(0 ,(range 10 20)))
  (place-channel-put (list-ref pls 1)
                     '(1 (5 10)))
  (displayln (map place-channel-get pls)))
