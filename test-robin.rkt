#lang racket
(require "lang/kara.rkt"
         "engine.rkt"
         "robin.rkt")

;; Testingu!
(def (fib n)
  (check-timer)
  (if (< n 2)
      n
    (+ (fib (- n 1))
       (fib (- n 2)))))

; Fib from 4 to 12
(def fib-gen1
  (generator ()
    (let loop ([n 4])
      (yield (fib n))
      (if (<= n 9)
          (loop (+ n 1))
        'DONE))))

; Fib from 12 to 3
(def fib-gen2
  (generator ()
    (let loop ([n 12])
      (yield (fib n))
      (if (>= n 4)
          (loop (- n 1))
        'DONE))))

(def g (gen-robin (list fib-gen1 fib-gen2)))
(gen-get g 22 (lam (x) (printf "~s " x)))


; For the stream
; Fib from 4 to 12
(def (fib-stream1 [n 4])
  (if (<= n 9)
      (stream-cons (fib n)
                   (fib-stream1 (+ n 1)))
    (stream-cons (fib n)
                 empty-stream)))

; Fib from 12 to 3
(def (fib-stream2 [n 12])
  (if (>= n 4)
      (stream-cons (fib n)
                   (fib-stream2 (- n 1)))
    (stream-cons (fib n)
                 empty-stream)))

(def s
  (stream-robin (list (fib-stream1)
                      (fib-stream2))))
(newline)
(display (stream->list s))
