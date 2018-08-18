#lang racket

(require "lang/kara.rkt"
         "enum.rkt")

(test-case
  "Just enumerate something"
  (def m (new mole%))
  (def g (kick-start m wf))
  (displayln (send (g) repr))
  (displayln (send (g) repr))
  (displayln (send (g) repr))
  (displayln (send (g) repr)))

;; (test-case
;;   "Trying to prove A->A"
;;   (def m (new mole%))
;;   (send m
;;     update-role 'ccs
;;                 Implication
;;                 (const "No way"))
;;   (send m
;;     update-path '(ccs ante)
;;                 A
;;                 (const "No way"))
;;   (send m
;;     update-path '(ccs csq)
;;                 A
;;                 (const "No way"))
;;   (get-one m entailment))
