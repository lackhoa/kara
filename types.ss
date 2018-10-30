(import (chezscheme)
        (kara-lang main)
        (mol))

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

(define category
  (list '(-> (morph 0 1 2)
            (-> (morph 3 2 4)
               (morph (comp 3 0) 1 4))
            #|Composition type|#)

        '(-> (im 1)
            (-> (= 0 2) (morph 1 0 2))
            #|Type of identity morphism|#)

        '(-> (= 0 1)
            (-> (morph 0 2 3)
               (-> (morph 1 4 5) (= 2 4))))

        '(-> (= 0 1)
            (-> (morph 0 2 3)
               (-> (morph 1 4 5) (= 3 5))))

        '(-> (im 0) (= (comp 0 1) 0)
            #|Left identity|#)

        '(-> (im 0) (= (comp 1 0) 0)
            #|Right identity|#)))
