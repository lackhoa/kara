(import (chezscheme)
        (kara-lang main))
(load "mol.ss")

;;; Utility functions


;;; Combinators
(define i '(-> 0 0))

(define k '(-> 0 (-> 1 0)))

(define s
  '(-> (-> 0 (-> 1 2))
      (-> (-> 0 1)
         (-> 0 2))))

(define p '(=> (-> 0 1) 0 1))

(define mp
  '(=> (=> 11 22 (-> 0 1))
      (=> 33 44 0)
      1))

;;; Some more combinators
(define w
  '(-> (-> 0 (-> 0 1))
      (-> 0 1)))

(define c
  '(-> (-> 0 (-> 1 2))
      (-> 1 (-> 0 2))))

(define b
  '(-> (-> 1 2)
      (-> (-> 0 1)
         (-> 0 2))))

(define and-elims
  (list (mk-proof '((and 0 1))
                  '0)
        (mk-proof '((and 0 1))
                  '1)))

(define equality
  (list '(: eq-trans
            (-> (: 0 11)
               (-> (: 1 22)
                  (-> (: 2 33)
                     (-> (= 0 1) (-> (= 1 2) (= 0 2)))))))

        '(: eq-sym
            (-> (: 0 11)
               (-> (: 1 22)
                  (-> (= 1 0) (= 0 1)))))

        '(: eq-refl
            (-> 0 (= 0 0)))

        '(: eq-func
            (-> (: 0 11)
               (-> (: 1 11)
                  (-> (: 2 prop)
                     (-> (: 3 prop)
                        (-> (: 4 (-> 11 prop))
                           (-> (= 0 1)
                              (-> (eval (4 0) 2)
                                 (-> (true 2)
                                    (-> (eval (4 1) 3)
                                       (true 3)))))))))))))

(define peano
  (list '(: zero nat)
        '(: succ
            (-> 0
               (-> (: 0 nat)
                  (: (++ 0) nat))))

        '(: succ-injection
            (-> (: 0 nat)
               (-> (: 1 nat)
                  (-> (= 0 1)
                     (= (++ 0) (++ 1))))))

        '(: succ-not-zero
            (-> (: 0 nat)
               (-> (not (= zero (++ 0))))))))

(define evaluation
  (list (mk-proof '((: 0 (-> 11 22))
                    (: 1 11)
                    (eval (0 1) 2))
                  (: 2 22))

        (mk-proof '()
                  '(: i (-> 0 0)))

        (mk-proof '()
                  '(: s (-> (-> 0 (-> 1 2))
                           (-> (-> 0 1)
                              (-> 0 2)))))

        (mk-proof '()
                  '(: b (-> (-> 1 2)
                           (-> (-> 0 1)
                              (-> 0 2)))))

        (mk-proof '()
                  '(: c (-> (-> 0 (-> 1 2))
                           (-> 1 (-> 0 2)))))))
