#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out))

(def (new-mole) (tag 'Mole (make-hash)))

(def (mole-member? mole atom)
  (let ([c-mole (content mole)])
    (hash-has-key? c-mole atom)))

; Get an atom, assuming it's in the molecule.
(def (mole-ref-atom path)
  (let ([c-mole (content mole)])
    (hash-ref c-mole path)))

(def (mole-members mole)
  (let ([c-mole (content mole)])
    (hash-keys c-mole)))

(def (mole-update mole path val)
  (if (typed-atom? val)
      (atom-union (mole-ref-atom path) val)
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
      ; Will return a completely empty molecule if the path
      ; is found nowhere, but that's fine.
      (let ([clone (make-hash)])
        (for-each (lam (p)
                    (hash-set! clone p (mole-ref-atom mole p)))
                  (filter (lam (s) (string-prefix? path s))
                          (mole-members mole)))
        (tag 'Mole clone))))
