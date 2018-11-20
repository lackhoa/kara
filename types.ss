(import (chezscheme)
        (kara-lang main))
(load "mol.ss")

;;; Utility functions
(define get-ccs
  (f> ref '[cdr car]))

(define get-prem
  (f>> (f> ref '[cdr cdr])
       (f> mol-<
           (lambda _  (list))  (lambda _  (list))
           identity)))

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

        (mk-proof '(= 0 0))

        (mk-proof '(= 0 2)
                  '(= 0 1) '(= 1 2))))

(define category
  (list (#|Left identity|#
         mk-proof '(= (compose 0 1) 0)
                  '(im 0))

        (#|Right identity|#
         mk-proof '(= (compose 1 0) 0)
                  '(im 0))))
