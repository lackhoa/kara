#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out))

; ---------------------------------
; Atoms
; ---------------------------------
; Atoms are like sets in the mathematical sense.
; The atom type has two constructors:
; (Implicit predicate)
; (Explicit set)
; Actually, empty molecules can also be considered atoms. Atoms and molecules are also called "aggregates", as they represent multiple possibilities.

(def (typed-atom? x)
  (or (tagged? x 'Implicit)
      (tagged? x 'Explicit)))

(def (atom-member? atom x)
     (case atom
       [(Implicit pred) (pred x)]
       [(Explicit set)  (set-member? set x)]))

(def (atom-intersect atom1 atom2)
  ; Empty molecules can actually be atoms. BUT it's an error to pass actual molecules in.
  (when (or (and (mole? atom1)
                 (not (mole-empty? atom1)))
            (and (mole? atom2)
                 (not (mole-empty? atom2))))
    (error "atom-intersect" "Expecting empty molecules" (list atom1 atom2)))

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

; ---------------------------------
; Molecules
; ---------------------------------
; Molecules are just hash tables containing atoms.

(def (mole? exp) (tagged? exp 'Mole))

(def (new-mole) (tag 'Mole (make-hash)))

(def (mole-empty? mole)
  (hash-empty? (content mole)))

(def (mole-member? mole atom)
  (hash-has-key? (content mole) atom))

; Get an atom, assuming it's in the molecule.
(def (mole-ref-atom path)
  (hash-ref (content mole) path))

; Get the names of all the atoms in this molecule
(def (mole-members mole)
  (hash-keys (content mole)))

(def (mole-copy mole)
  (let ([c-mole (content mole)]
    (tag 'Mole (hash-copy c-mole)))))

; It's like intersection, but not quite the same.
(def (mole-update mole path val)
  (if (typed-atom? val)
      (let ([clone (hash-copy (content mole))]
        (hash-set! clone
                   path
                   (atom-intersect (mole-ref-atom path) val))
        (tag 'Mole clone)))
    ; Molecule type
    (let* ([pad (lam (p) (if (non-empty-string? path)
                             (append-string path "/" p)
                           p))]
           [strip (lam (p)
                    (substring p (+ (length path) 1)))]  ; '+1' for the slash
           [mems (mole-members mole)]
           [v-mems (map pad (mole-members val))]
           [result (make-hash)])
      (for-each (lam (p)
                  (hash-set! result
                    p
                    (atom-union (mole-ref-atom mole p)
                                (mole-ref-atom val (strip p)))))
                (set-union mems v-mems))
      (for-each (lam (p)
                  (if (mole-member? mole p)
                      (hash-set! result p (mole-ref-atom mole p))
                    (hash-set! result p (mole-ref-atom val (strip p)))))
                (set-symmetric-difference mems v-mems))
      (tag 'Mole result))))

(def (mole-ref path)
  (if (mole-member? mole path)
      ; An atom of that path is found.
      (mole-ref-atom path)
      ; Will return a completely empty molecule if the path is found nowhere, but that's fine.
      (let ([clone (make-hash)])
        (for-each (lam (p)
                    (hash-set! clone p (mole-ref-atom mole p)))
                  (filter (lam (s) (string-prefix? path s))
                          (mole-members mole)))
        (tag 'Mole clone))))
