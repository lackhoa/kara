#! /usr/bin/racket
#lang racket

(require "lang/kara.rkt"
         "enum.rkt"
         "engine.rkt")

(def (trivial)
  (def m (new mole%))
  (send m set-type wf)

  (bfs m))

;; (trivial)

(def (ai)
  (def m (new mole%))

  (send m set-type entailment)

  (update-macro
   m
   ([0] Implication))

  (partition-macro
   m
   ([0 0] [0 1]))

  (bfs m))

(ai)
