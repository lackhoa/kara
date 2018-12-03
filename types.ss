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
  (list (mk-proof '(= 0 1)
                  '(= 1 0))

        (mk-proof '(= 0 2)
                  '(= 0 1) '(= 1 2))))

(define category
  (list (mk-proof '(= 1 3)
                  '(-> 0 1 2)
                  '(-> 0 3 4))

        (mk-proof '(= 2 4)
                  '(-> 0 1 2)
                  '(-> 0 3 4))

        (mk-proof '(= (c (c 0 1) 2)
                      (c 0 (c 1 2)))
                  '(-> 2 3 4)
                  '(-> 1 4 5)
                  '(-> 0 5 6))

        (mk-proof '(= (c 0 (- 0)) 2)
                  '(i-> 0)
                  '(-> 0 1 2))

        (mk-proof '(= (c (- 0) 0) 1)
                  '(i-> 0)
                  '(-> 0 1 2))))

(define circuit
  (list
   ;; (mk-proof '(V 0 1 (- 2))
   ;;           '(V 1 0 2))

   ;; (mk-proof '(V 0 1 (+ 2 3))
   ;;           '(V 0 4 2)
   ;;           '(V 4 1 3))

   (mk-proof '(res 0 1 2)
             '(res 0 2 1))

   (mk-proof '(bat (- 0) 2 1)
             '(bat 0 1 2)
             '(;; meta-logical constraint
               !neg 0))
   (mk-proof '(bat 0 2 1)
             '(bat (- 0) 1 2))

   (mk-proof '(any-elm 0 1 2)
             '(res 0 1 2))
   (mk-proof '(any-elm 0 1 2)
             '(bat 0 1 2))

   (mk-proof '(path (0) 1 2)
             '(any-elm 0 1 2))

   (mk-proof '(path (0 4 . 1) 2 3)
             '(any-elm 0 2 4)
             '(path 1 4 3))
   ))

(define ca49
  (list '(=> (all-elm p1 r1))
        '(=> (all-elm p2 r1 r4 b1))
        '(=> (all-elm p3 r1 r2 r4))
        '(=> (all-elm p4 r3 r4 r5))

        '(=> (res r1 p1 p3))
        '(=> (res r2 p2 p3))
        '(=> (res r3 p1 p4))
        '(=> (res r4 p2 p4))
        '(=> (res r5 p3 p4))
        '(=> (bat b1 p1 p2))))
