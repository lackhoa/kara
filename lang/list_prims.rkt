; This file preserves the list primitives that will be shadowed by "seq.rkt"

#lang racket
(require "macro.rkt")
(provide (all-defined-out))

(def lcar car)
(def lcdr cdr)
(def lcadr cadr)
(def llength length)
(def lrange range)
(def lappend append)
(def lfilter filter)
(def lmap map)
