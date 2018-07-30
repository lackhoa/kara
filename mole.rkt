#lang racket
(require "lang/kara.rkt")
(provide mole%)

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
    (let* ([m-focus (mole-ref path)]
           [m-focus-mems (mole-members m-focus)]
           [val-mems (mole-members val)]
           [same-keys (set-union m-focus-mems val-mems)]
           [exclusive-keys (set-symmetric-difference m-focus-mems val-mems)]
           [result (make-hash)])
      (for-each (lam (p)
                  (hash-set! result
                    p
                    (atom-union (mole-ref-atom m-focus p)
                                (mole-ref-atom val p))))
                same-keys)
      (for-each (lam (p)
                  (if (mole-member? m-focus p)
                      (hash-set! result p (m-ref-atom m-focus p))
                    (hash-set! result p (m-ref-atom val p))))
                exclusive-keys)
      (tag 'Mole result))))

(define/public (mole-ref path)
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
