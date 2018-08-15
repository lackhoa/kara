#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         rackunit)

(def m1
  (new mole% [data-i 'gaylord]
             [children-i '((a . x) (b . y) (c . z))]))
(check-eq? (send m1 get-data)
           'gaylord)
(check-eq? (send m1 ref 'a)
           'x)
(check-equal? (send m1 get-roles)
              '(a b c))
