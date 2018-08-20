#lang racket

(require "lang/kara.rkt"
         "enum.rkt")

; This should now spits out generic values.
(def (enum1)
  (def m (new mole%))
  (def g (kick-start m wf))
  (gen-get g 10))

;; (enum1)

(def (proof1)
  (def m (new mole%))
  (update-macro m
    (ccs_csq  A)
    (ccs_ante A)
    (ccs      Implication))

  (def g (kick-start m entailment))
  (gen-get g 3))

;; (proof1)

; (A -> (A -> B)) -> (A -> B)
(def (AW-proof)
  (def m (new mole%))
  (update-macro m
    (ccs               Implication)
    (ccs_ante          Implication)
    (ccs_ante_ante     A)
    (ccs_ante_csq      Implication)
    (ccs_ante_csq_ante A)
    (ccs_ante_csq_csq  B)
    (ccs_csq           Implication)
    (ccs_csq_ante      A)
    (ccs_csq_csq       B))
  (def g
    (kick-start m entailment))
  (gen-get g 1))

;; (AW-proof)
