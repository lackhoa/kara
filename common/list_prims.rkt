#lang racket
(require "kara.rkt")
(provide lcar lcdr llength lrange)
; This file preserves the list primitives that will be shadowed by seq.rkt

(def lcar car)
(def lcdr cdr)
(def llength length)
(def lrange range)
