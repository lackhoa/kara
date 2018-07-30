#lang racket
(require "lang/kara.rkt"
         "atom.rkt"
         rackunit)

(def even-atom (tag 'Implicit even?))
(check-equal? #f (atom-member? 89 even-atom) "Implicit atom 1")
(check-equal? #t (atom-member? 74 even-atom) "Implicit atom 2")

(def atom-9 (tag 'Explicit (list->set (seq->list (range 0 9)))))
(check-equal? #t (atom-member? 8 atom-9) "Explicit atom 2")
(check-equal? #f (atom-member? 59 atom-9) "Explicit atom 2")


