#lang racket
(require "lang/kara.rkt"
         "atom.rkt"
         rackunit)

; -----------------------------------
; Atom
; -----------------------------------
(def even-atom (make-implicit even?))
(check-equal? #f (atom-member? even-atom 89) "Implicit atom 1")
(check-equal? #t (atom-member? even-atom 74) "Implicit atom 2")

(def atom-9 (make-explicit (list->set (seq->list (range 0 9)))))
(check-equal? #t (atom-member? atom-9 8) "Explicit atom 2")
(check-equal? #f (atom-member? atom-9 59) "Explicit atom 2")

(def even9 (atom-intersect even-atom atom-9))
(check-equal? #f (atom-member? even9 7) "Intersection 1")
(check-equal? #t (atom-member? even9 4) "Intersection 2")
