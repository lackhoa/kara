#lang racket
(require "lang/kara.rkt")
(provide atom-member?)

; Atoms are like the mathematical sense of sets
; The atom type has two constructors:
; (Implicit predicate)
; (Explicit set)

(def (atom-member? x atom)
     (case atom
       [(Implicit pred) (pred x)]
       [(Explicit set)  (set-member? set x)]))
