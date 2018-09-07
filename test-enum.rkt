#! /usr/bin/racket
#lang racket

(require "lang/kara.rkt"
         "enum.rkt"
         "engine.rkt")

(def (trivial)
  (def m (new mole%))
  (send m update-role 'type wf)

  (bfs m))

;; (trivial)

(def (proof1)
  (def m (new mole%))
  (send m update-role
    'type entailment)
  (update-ctors m
                (ccs Implication))
  (send m sync-path
    '[ccs ante]
    '[ccs csq])
  (send m update-path
    '[ccs ante ctor] '?DATA)
  (send m update-path
    '[ccs csq ctor] '?DATA)
  (send (send m ref '[ccs ante ctor]) mark-no-touch)
  (send (send m ref '[ccs csq ctor]) mark-no-touch)

  (bfs m))

(proof1)

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

  (bfs m))

;; (AW-proof)

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

  (bfs m))

;; (reverse-proof)
