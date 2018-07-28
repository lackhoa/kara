#lang racket

(require "kara_macro.rkt"
         "utils.rkt"
         rackunit)

(check-equal? '(S z) (tag 'S 'z) "Tagging")

