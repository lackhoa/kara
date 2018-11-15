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

(define misc
  (list '(: be-true
            (-> (: 11 *)
               (-> (: 0 11)
                  (true 11))))))

(define peano
  (list '(zero)

        '(0 1 (= 0 1)
            (= (++ 0) (++ 1)))

        '(0 (= (++ 0) zero)
            f)

        '(0 (=> (0 zero) 1) 1
            ((=> (0 f) 8) 8
             (=> (0 (++ f)) 9)
             9)
            (#|3 is the proposition given by applying 0 to anything|#
             => (0 2) 3)
            3)))

(define evaluation
  (list '((=> 0 2)  (=> 1 3)  (=> (2 3) 4)
          (=> (0 1) 4))

        '((=> (i 0) 0))

        '((=> (0 2) 3)  (=> (1 2) 4)  (=> (3 4) 5)
          (=> (s 0 1 2)
             5))

        '((=> (1 2) 3)  (=> (0 3) 4)
          (=> (b 0 1 2)
             4))

        '((=> (0 2) 3)  (=> (3 1) 4)
          (=> (c 0 1 2)
             4))))
