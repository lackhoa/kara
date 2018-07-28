#lang racket
(require "lang/kara.rkt"
         "atom.rkt"
         rackunit)

(def even-atom (tag 'Implicit even?))
(check-equal? #f (in-atom? 89 even-atom) "Implicit atom 1")
(check-equal? #t (in-atom? 74 even-atom) "Implicit atom 2")

(def atom-9 (tag 'Explicit (tag 'Set (range 0 9))))
(check-equal? #t (in-atom? 8 atom-9) "Explicit atom 2")
(check-equal? #f (in-atom? 59 atom-9) "Explicit atom 2")


