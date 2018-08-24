#lang racket

(require "lang/kara.rkt"
         "enum.rkt"
         "engine.rkt")

(def (enum-test)
  ; Now, we assume these symbols to be
  ; well-formed formulas (they aren't).
  (def m (new mole%))
  (send m
    update-role 'type wf)

  (enum1 m))

;; (enum-test)

(def (proof1)
  (def m (new mole%))
  (send m
    update-role 'type entailment)

  (update-ctors m
    (ccs       Implication)
    (ccs_ante  A)
    (ccs_csq   A))

  (enum1 m))

;; (proof1)

; If you don't have AB and AC, this proof is impossible
; (A -> (A -> B)) -> (A -> B)
(def (AW-proof)
  (def m (new mole%))
  (send m
    update-role 'type entailment)

  (update-ctors m
    (ccs                Implication)
    (ccs_ante           Implication)
    (ccs_ante_ante      A)
    (ccs_ante_csq       Implication)
    (ccs_ante_csq_ante  A)
    (ccs_ante_csq_csq   B)
    (ccs_csq            Implication)
    (ccs_csq_ante       A)
    (ccs_csq_csq        B))

  (enum1 m))

(time (AW-proof))

; A -> ((A -> B) -> B)
(def (reverse-proof)
  (def m (new mole%))
  (send m
    update-role 'type entailment)

  (update-ctors m
    (ccs                Implication)
    (ccs_ante           A)
    (ccs_csq            Implication)
    (ccs_csq_ante       Implication)
    (ccs_csq_ante_ante  A)
    (ccs_csq_ante_csq   B)
    (ccs_csq_csq        B))

  (enum1 m))

;; (reverse-proof)
