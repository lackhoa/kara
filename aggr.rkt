#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out))

; ---------------------------------
; Atoms
; ---------------------------------
; Atoms are like sets in the mathematical sense.
; The atom type has two constructors:
; (Implicit premole-dicate)
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
; Also, they can have equating mole-eqs.

(def (mole? exp) (tagged? exp 'Mole))

(def (make-mole ht eqs)
  (tag 'Mole (list ht eqs)))

(def (new-mole) (make-mole (make-hash) null))

(def (mole-dic mole) (car  (tag-body mole)))
(def (mole-eqs mole) (cadr (tag-body mole)))

(def (mole-empty? mole)
  (hash-empty? (mole-dic mole)))

(def (mole-member? mole atom)
  (hash-has-key? (mole-dic mole) atom))

; Get an atom, assuming it's in the molecule.
(def (mole-ref-atom path)
  (hash-ref (mole-dic mole) path))

; Get the names of all the atoms in this molecule
(def (mole-members mole)
  (hash-keys (mole-dic mole)))

(def (mole-copy mole)
  (tag 'Mole (list (hash-copy (mole-dic mole))
                   (mole-eqs mole)))

(def (pad path relative)
    (if (non-empty-string? path)
        (append-string path "/" relative)
      relative))

(def (pad-eq path eq)
  (lmap (lam (x) (pad path x))
        eq))

; It's like intersection, but not quite the same.
(def (mole-update mole path val)
  (if (typed-atom? val)
      (let ([dic (hash-copy (mole-dic mole))]
        (hash-set! dic
                   path
                   (atom-intersect (mole-ref-atom path) val))
        (make-mole (apply-eqs! dic
                               (mole-eqs mole)
                               (list path))
                   (mole-eqs mole))))
    ; Molecule type
    (let* ([strip (lam (p)
                    (substring p (+ (length path) 1)))]  ; '+1' for the slash
           ; Transform the relative paths to absolute paths.
           [v-mems (map (lam (r) (pad path r))
                        (mole-members val))]
           [dic (make-hash)])
      ; Members in both molecules.
      (for-each (lam (p)
                  (hash-set! dic
                    p
                    (atom-intersect (mole-ref-atom mole p)
                                    (mole-ref-atom val (strip p)))))
                (set-intersect (mole-members mole) v-mems))
      ; Members in only one molecule
      (for-each (lam (p)
                  (if (mole-member? mole p)
                      (hash-set! dic p (mole-ref-atom mole p))
                    (hash-set! dic p (mole-ref-atom val (strip p)))))
                (set-symmetric-difference (mole-members mole) v-mems))
      (make-mole (apply-eqs! dic
                             (mole-eqs mole)
                             v-mems)
                 mole-eqs)))))

; We make a choice to pass on the hashtable directly
(def (mole-update-eqs mole path eqs)
  (let ([new-eqs (map (lam (r) (pad-eq path r))
                      (mole-eqs mole))])
    (make-mole (mole-dic mole)
               (lappend (mole-eqs mole) new-eqs))))

; Mutate `dic`. It's simple because they're all equalities.
(def (apply-eqs! dic eqs updated)
  (for-each
    (lam (path)
      (when (singleton? (hash-ref dic path))
        (for-each
          (lam (eq)
            (when (set-member? eq path)
                (for-each (lam (p)
                            (hash-set! p (atom-intersect (hash-ref dic path)
                                                         (hash-ref dic p))))
                          eq)))
          eqs)))
    updated))

; QUESTIONABLE
(def (mole-ref path)
  (if (mole-member? mole path)
      ; An atom of that path is found.
      (mole-ref-atom path)
      ; Will return a completely empty molecule if the path is found nowhere, but that's fine.
      (let ([dic (make-hash)])
        (for-each (lam (p)
                    (hash-set! dic p (mole-ref-atom mole p)))
                  (filter (lam (s) (string-prefix? path s))
                          (mole-members mole)))
        (make-mole dic (mole-eqs mole)))))  ; Wait, what?
