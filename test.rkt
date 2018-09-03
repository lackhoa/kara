#lang racket
(require "lang/kara.rkt")

(def NUM 1000)
(def REF 500)

(def ls
  (for/list ([i (range NUM)])
    (cons i (* i i))))

(def ht (make-hasheq))
(for ([i (range NUM)])
  (hash-set! ht i (* i i)))

(time (assq REF ls))
(time (hash-ref ht REF))
