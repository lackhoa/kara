#lang racket

(require "lang/kara.rkt"
         "enum.rkt")

(def (enum1)
  (def m (new mole%))
  (def g (kick-start m wf))
  (gen-get g 10))

(enum1)

(def (proof1)
  (def m (new mole%))
  (send m
    update-role 'ccs Implication)
  (send m
    update-path '(ccs ante) A-Sym)
  (send m
    update-path '(ccs csq) A-Sym)
  (def g (kick-start m entailment))
  (gen-get g 1))

;; (proof1)
