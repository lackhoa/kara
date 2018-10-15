#lang racket
(require "lang/kara.rkt")

(provide main)

(def (main)

  (def p1
    (place ch
      (for ([i  (in-range 10)])
        (place-channel-put ch i)
        (sleep 0.1))))

  (def p2
    (place ch
      (for ([i  (in-range 10 20)])
        (place-channel-put ch i)
        (sleep 0.1))))

  (let loop ()
    (match (sync/timeout 1 p1 p2)
      [#f  (displayln "I'm done!")]
      [a   (printf "~a " a)
           (loop)])))
