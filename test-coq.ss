(load "coq.ss")

(prove plus0r ind)

#!eof
(test "Freshen"
      (freshen "X" '(X X*))
      'X**)

(test "Substitution"
      (substitute 'Y* '(+ Y 1) '(forall Y (= Y* Y)))
      '(forall Y** (= (+ Y 1) Y**)))

(test "Instantiation"
      (inst '(+ X 1) '(forall Y (and (= Y* Y) (forall Y (= Y Y)))))
      '(and (= Y* (+ X 1)) (forall Y (= Y Y))))

(test "alpha-equiv? 1"
      (alpha-equiv? '(forall X (s X)) '(forall Y (s Y)))
      #t)

(test "alpha-equiv? 2"
      (alpha-equiv? '(forall X (s X)) '(forall Y (s X)))
      #f)
