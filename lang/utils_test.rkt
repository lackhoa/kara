#lang racket

(require "macro.rkt"
         "utils.rkt"
         rackunit)

(check-equal? '(S z) (tag 'S 'z) "Tagging")

