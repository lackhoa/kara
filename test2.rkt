#lang racket

(let ([pls (for/list ([i (in-range 2)])
             (dynamic-place "test.rkt" 'place-main))])
  (for ([i (in-range 2)]
        [p pls])
    (place-channel-put p i)
    (printf "~a\n" (place-channel-get p)))
  (map place-wait pls)
  (sleep 10))
