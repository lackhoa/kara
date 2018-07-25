; Atoms are like the mathematical sense of sets
; The atom type has two constructors:
; Implicit predicate
; Explicit set

(def (in-atom? x atom)
     (case atom
       [(Implicit pred) (pred x)]
       [(Explicit set)  (in-set? x set)]))




