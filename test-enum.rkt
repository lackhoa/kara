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

  (let/ec escape
    (stream-for-each
     (lam (n)
       (printf "Try#: ~s\n" n)
       ((proc->engine (lam ()
                        (gen-get (enum m 5)
                                 1
                                 (lam (x) (display x)
                                   (escape 'DONE)))))
          30
          list
          list))
     (in-range 10000))))

;; (AW-proof)


; (((A -> B) -> (B -> C)) -> (A -> C))
(def (improv-proof)
  (def m (new mole%))
  (send m
    update-role 'type entailment)

  (update-ctors m
    (ccs                 Implication)
    (ccs_ante            Implication)
    (ccs_ante_ante       Implication)
    (ccs_ante_csq        Implication)
    (ccs_ante_ante_ante  A)
    (ccs_ante_ante_csq   B)
    (ccs_ante_csq_ante   B)
    (ccs_ante_csq_csq    C)
    (ccs_csq_ante        A)
    (ccs_csq_csq         C))

  (let/ec escape
    (stream-for-each
     (lam (n)
       (printf ".")
       ((proc->engine (lam ()
                        (gen-get (enum m 5)
                                 1
                                 (lam (x) (display x)
                                   (escape 'DONE)))))
          30
          list
          list))
     (in-range 10000))))

(improv-proof)
