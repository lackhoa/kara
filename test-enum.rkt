#lang racket

(require "lang/kara.rkt"
         "enum.rkt")

(def (enum-test)
  ; Now, we assume these symbols to be
  ; well-formed formulas (they aren't).
  (def m (new mole%))
  (send m
    update-role 'type wf)

  (gen-get (enum m) 2))

;; (enum-test)

(def (proof1)
  (def m (new mole%))
  (send m
    update-role 'type entailment)

  (update-ctors m
    (ccs       Implication)
    (ccs_ante  A)
    (ccs_csq   A))

  (gen-get (enum m) 1))

;; (proof1)

; (A -> (A -> B)) -> (A -> B)
(def (AW-proof)
  (def m (new mole%))
  (send m
    update-role 'type entailment)

  (update-ctors m
    (ccs               Implication)
    (ccs_ante          Implication)
    (ccs_ante_ante     A)
    (ccs_ante_csq      Implication)
    (ccs_ante_csq_ante A)
    (ccs_ante_csq_csq  B)
    (ccs_csq           Implication)
    (ccs_csq_ante      A)
    (ccs_csq_csq       B))

  (gen-get (enum m) 1))

(AW-proof)
