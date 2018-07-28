#lang racket

(require "kara_macro.rkt"
         "seq.rkt"
         "set.rkt"
         rackunit)

(check-equal? #t
              (in-set? 2 (seq->set (list 1 2 3 4)))
              "In set")

(check-equal? #f
              (in-set? 5 (seq->set (range 1 4)))
              "In set with range => #f")

(check-equal? '(Set (1 2 9))
              (intersection-set (seq->set (range 0 11))
                                (seq->set (list 1 2 9 11)))
              "Intersection")

(def superset (union-set (seq->set (range 3 9))
                         (seq->set (list 0 1 2 9 10 11 12))))
(check-equal? 13
              (case superset [(Set sq) (length sq)])
              "Union")
