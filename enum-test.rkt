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
  (send m
    update-role 'ccs Implication)
  (send m
    update-path '(ccs ante) A)
  (send m
    update-path '(ccs csq) A)
  (def g (kick-start m entailment))
  (gen-get g 3))

(proof1)
