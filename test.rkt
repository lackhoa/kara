#lang racket
(require "lang/kara.rkt")

(def-mem (mfib n)
  (cond
   [(< n 1) 1]
   [else (+ (mfib (- n 1)) (mfib (- n 2)))]))

(time (mfib 1000))
