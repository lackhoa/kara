(import (chezscheme)
        (kara-lang main))
(load "mol.ss")

;;; Utility functions
(define get-ccs
  (f> ref '[cdr car]))

(define get-prem
  (f> ref '[cdr cdr]))

(define mk-proof
  (lambda (conclusion . premises)
    `(=> ,conclusion
        ,@(let loop ([premises  premises]
                     [i         100])
            (if (null? premises)  (list)
                (cons `(=> ,(car premises) . ,i)
                      (loop (cdr premises)
                            (+ i 1))))))))

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
  (list (mk-proof '(prop (= 0 1))
                  '(type 2) '(2 0) '(2 1))

        (mk-proof '(= 0 1)
                  '(type 2) '(2 0) '(2 1)
                  '(= 1 0))

        (mk-proof '(= 0 0)
                  '(type 1) '(1 0))

        (mk-proof '(= 0 2)
                  '(type 3) '(3 0) '(3 1) '(3 2)
                  '(= 0 1) '(= 1 2))

        (;; Second-order rule
         mk-proof 3
                  '(type 4) '(4 0) '(4 1) '(prop 2) '(prop 3)
                  '(= 0 1)  '(subs 5 0 2)  2  '(subs 5 1 3))))

(define category
  (list (mk-proof '(type map))

        (mk-proof '(map (compose 0 1))
                  '(map 0) '(map 1))

        (mk-proof '(prop (im 0))
                  '(map 0))

        (#|Left identity|#
         mk-proof '(= (compose 0 1) 0)
                  '(im 0) '(map 1))

        (#|Right identity|#
         mk-proof '(= (compose 1 0) 0)
                  '(im 0) '(map 1))))

(define substitution
  (list (mk-proof '(prop (subs 0 1 2)))

        (;; subs: substitute argument for star
         mk-proof '(subs * 0  0))

        (;; subs: ignore constants
         mk-proof '(subs (const 0) 1  0))

        (;; subs: recursively substitute for lists
         mk-proof '(subs (:: 0 1) 2  (3 . 4))
                  '(subs 0        2  3)
                  '(subs 1        2  4))))
