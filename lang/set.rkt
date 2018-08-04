#lang racket
(require "macro.rkt"
         "seq.rkt")
(provide (all-defined-out))

(def (set-filter pred set)
  (list->set (lfilter pred (set->list set))))
