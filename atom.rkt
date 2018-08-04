#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out))

; ---------------------------------
; Atoms
; ---------------------------------
; Atoms are sets in the mathematical sense.
; The atom type has two constructors:
; (Implicit predicate)
; (Explicit set)

(def (typed-atom? x)
  (or (tagged? x 'Implicit)
      (tagged? x 'Explicit)))

(def (make-implicit pred)
  (tag 'Implicit pred))

(def (make-explicit set)
  (tag 'Explicit set))

(def (atom-member? atom x)
     (case atom
       [(Implicit pred) (pred x)]
       [(Explicit set)  (set-member? set x)]))

(def (atom-intersect atom1 atom2)
  (case atom1
    [(Implicit pred1)
     (case atom2
       [(Implicit pred2) (make-implicit (and-pred pred1 pred2))]
       [(Explicit set2)  (make-explicit (set-filter pred1 set2))])]
    [(Explicit set1)
     (case atom2
       [(Implicit pred2) (make-explicit (set-filter pred2 set1))]
       [(Explicit set2)  (make-explicit (set-intersect set1 set2))])]))
