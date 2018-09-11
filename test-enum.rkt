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

;; (displayln (ai))

(def (modus)
  ;; (=> (-> A
  ;;         (-> (-> A B)
  ;;             B)))
  (def m (new mole%))

  (send m set-type entailment)

  (update-macro
   m
   ([0]     Implication)
   ([0 1]   Implication)  ; (-> (-> A B) B)
   ([0 1 0] Implication)) ; (-> A B)

  (partition-macro
   m
   ([0 0]   [0 1 0 0])
   ([0 1 1] [0 1 0 1]))

  (displayln m)
  (bfs m))

;; (displayln (modus))

(def (enum)
  (def m (new mole%))
  (send m set-type entailment)
  (gen->list (bfs-enum m) 20))

(let* ([enum-res (enum)]
       [enum-cl  (cleanup enum-res)])
  (pdisplay enum-cl))
