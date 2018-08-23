#lang racket

(require "lang/kara.rkt"
         "enum.rkt"
         "engine.rkt")

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

  (let/ec escape
    (stream-for-each
     (lam (n)
       (printf "Try#: ~s\n" n)
       ((proc->engine (lam ()
                        (gen-get (enum m 5)
                                 1
                                 (lam (x) (display x)
                                   (escape (void))))))
          30
          list
          list))
     (in-range 10000))))

;; (proof1)

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

  ;; (let/ec escape
  ;;   (stream-for-each
  ;;    (lam (n)
  ;;      (printf "Try#: ~s\n" n)
  ;;      ((proc->engine (lam ()
  ;;                       (gen-get (enum m 10)
  ;;                                1
  ;;                                (lam (x) (display x)
  ;;                                  (escape 'DONE)))))
  ;;         3000
  ;;         list
  ;;         list))
  ;;    (in-range 1)))
  (gen-get (enum m) 1))

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

  (gen-get (enum m) 1))

(reverse-proof)
