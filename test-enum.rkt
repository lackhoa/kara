#lang racket

(require "lang/kara.rkt"
         "enum.rkt")

; This should now spits out generic values.
(def (enum1)
  (def m (new mole%))
  (send m update-role 'ctor wf)
  (gen-get (enum m) 10))

(enum1)

(def (proof1)
  (def m (new mole%))
  (update-roles m
    (ccs Implication))
  (send m
    sync ccs_ante ccs_csq)

  (gen-get (enum m) 3))

;; (proof1)

; (A -> (A -> B)) -> (A -> B)
(def (AW-proof)
  (def m (new mole%))
  (update-roles m
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

;; (AW-proof)
