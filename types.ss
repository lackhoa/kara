(import (chezscheme)
        (kara-lang main)
        (mol))

;;; Utility functions
(define conclusion
  (f> ref '[2]))

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

(define equality
  (list '(= 0 0)
        '(-> (= 1 0) (= 0 1))
        '(-> (= 0 1)
            (-> (= 1 2) (= 0 2)))))

(define and-rules
  (list '(-> 0 (-> 1 (and 0 1)))
        '(-> (and 0 1) 0)
        '(-> (and 0 1) 1)))

(define category
  (list '(-> (im 0)
            (and (= (compose 0 1) 0)
               (= (compose 1 0) 0))
            #|Left & right identity|#)

        '(-> (and (= (compose 0 (pvar 1)) 0)
               (= (compose (pvar 1) 0) 0))
            (im 0)
            #|The demand for identity morphism|#)))
