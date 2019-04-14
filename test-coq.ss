(load "coq.ss")

;; (test "Freshen"
;;       (freshen "x" '(x x*))
;;       'x**)

;; (test "Substitution"
;;       (sub 'y* '(+ y (1)) '(forall y (= y* y)))
;;       '(forall y** (= (+ y (1)) y**)))

;; (test "Instantiation"
;;       (inst '(forall y (and (= y* y) (forall y (= y y)))) '(+ x (1)))
;;       '(and (= y* (+ x (1))) (forall y (= y y))))

;; (test "Introduce variable"
;;       (intro (init-env '(forall x (= x x))))
;;       '((x) () (= x x) ()))

;; (test "plus-comm test"
;;       ((go
;;         ind done intro intro ind done intro intro
;;         (rwl '(s (+ n* (s n))))
;;         (rwr '(s (+ n (s n*))))
;;         inj
;;         (rwl '(s (+ n n*)))
;;         (rwr '(s (+ n* n)))
;;         inj done)
;;        (init-env plus-comm))
;;       terminal-env)

(test "Short-hand"
      (inst
       '(forall f (exists g (and (1 (c g f))
                       (1 (c f g)))))
       'g))
