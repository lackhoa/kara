#lang racket
(require "lang/kara.rkt"
         "engine.rkt"
         "robin.rkt")

;; Testingu!
(def (tfib n)
  (check-timer)
  (if (< n 2)
      n
    (+ (tfib (- n 1))
       (tfib (- n 2)))))

; Tfib from 4 to 12
(def (tfib-gen1)
  (generator ()
    (let loop ([n 4])
      (yield (tfib n))
      (if (<= n 11)
          (loop (+ n 1))
        'DONE))))

; Tfib from 12 to 3
(def (tfib-gen2)
  (generator ()
    (let loop ([n 12])
      (yield (tfib n))
      (if (>= n 4)
          (loop (- n 1))
        'DONE))))

(def g1 (gen-robin (list (tfib-gen1)
                         (tfib-gen2))))
(def g2 (gen-robin (list (tfib-gen1)
                         (tfib-gen2))))
(def h (gen-robin (list g1 g2)))
(gen-get h 100 (lam (x) (printf "~s " x)))


; For the stream
; Tfib from 4 to 12
(def (tfib-stream1 [n 4])
  (if (<= n 11)
      (stream-cons (tfib n)
                   (tfib-stream1 (+ n 1)))
    (stream-cons (tfib n)
                 empty-stream)))

; Tfib from 12 to 3
(def (tfib-stream2 [n 12])
  (if (>= n 4)
      (stream-cons (tfib n)
                   (tfib-stream2 (- n 1)))
    (stream-cons (tfib n)
                 empty-stream)))

(def (s1)
  (stream-robin (list (tfib-stream2)
                      (tfib-stream1))))

(def (s2)
  (stream-robin (list (s1) (s1))))

(newline)
(stream->list (s2))
