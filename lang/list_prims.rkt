; This file preserves the list primitives that will be shadowed by "seq.rkt"

#lang racket
(require "kara_macro.rkt")
(provide (all-defined-out))

(def lcar car)
(def lcdr cdr)
(def llength length)
(def lrange range)
(def lappend append)
(def lfilter filter)
(def lmap map)
