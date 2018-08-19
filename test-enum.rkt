#lang racket

(require "lang/kara.rkt"
         "enum.rkt")

(def (enum1)
  (def m (new mole%))
  (def g (kick-start m wf))
  (gen-get g 10))

;; (enum1)

(def (proof1)
  (def m (new mole%))
  (update-macro m
    (ccs_csq A-Sym)
    (ccs_ante A-Sym)
    (ccs Implication))

  (def g (kick-start m entailment))
  (gen-get g 3))

;; (proof1)

;(A -> (A -> B)) -> (A -> B)
(def (AW-proof)
  (def m (new mole%))
  (update-macro m
    (ccs               Implication)
    (ccs_ante          Implication)
    (ccs_ante_ante     A-Sym)
    (ccs_ante_csq      Implication)
    (ccs_ante_csq_ante A-Sym)
    (ccs_ante_csq_csq  B-Sym)
    (ccs_csq           Implication)
    (ccs_csq_ante      A-Sym)
    (ccs_csq_csq       B-Sym))
  (def g (kick-start m entailment))
  (gen-get g 1))

(AW-proof)
