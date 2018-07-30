#lang racket
(require "lang/kara.rkt")
(provide atom-member?)

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

(def (atom-union atom1 atom2)
  (case atom1
    [(Implicit pred1)
     (case atom2
       [(Implicit pred2)
        (tag 'Implicit (lam (x)
                        (and (pred1 x) (pred2 x))))]
       [(Explicit set2)
        (tag 'Explicit (set (lfilter
                              (lam (x) (atom-member? pred1 x)
                              set2))))])]
    [(Explicit set1)
     (case atom2
       [(Implicit pred2) (atom-union atom2 atom1)]
       [(Explicit set2)  (tag 'Explicit (set-union set1 set2))])]))
