#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out))

; Atoms are like the mathematical sense of sets
; The atom type has two constructors:
; (Implicit predicate)
; (Explicit set)

(def (typed-atom? x)
  (or (tagged? x 'Implicit)
      (tagged? x 'Explicit)))

(def (atom-member? atom x)
     (case atom
       [(Implicit pred) (pred x)]
       [(Explicit set)  (set-member? set x)]))

(def (atom-intersect atom1 atom2)
  ; Some molecule checking code here!!!
  (case atom1
    [(Mole m1) atom2]
    [(Implicit pred1)
     (case atom2
       [(Mole m2) atom1]
       [(Implicit pred2) (tag 'Implicit (and-pred pred1 pred2))]
       [(Explicit set2)  (tag 'Explicit (set-filter pred1 set2))])]
    [(Explicit set1)
     (case atom2
       [(mole m2) atom1]
       [(Implicit pred2) (tag 'Explicit (set-filter pred2 set1))]
       [(Explicit set2)  (tag 'Explicit (set-intersect set1 set2))])]))
