(import (chezscheme)
        (kara-lang main))
(load "mol.ss")

;;; Utility functions
(define get-ccs
  (f> ref '[1]))

(define get-prem
  (f>> (f> ref '[0])
       (f> mol-<
           (lambda _         (list))
           (lambda (_ kids)  kids))))

(define mk-proof
  (lambda (premise conclusion)
    `(=> (list ,@(let loop ([premise  premise]
                           [i        100])
                  (if (null? premise)  (list)
                      (cons `(=> ,i ,(car premise))
                            (loop (cdr premise)
                                  (+ i 1))))))
        ,conclusion)))

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
  (list (mk-proof '((= 0 1) (= 1 2))
                  '(= 0 2))

        (mk-proof '((= 1 0))
                  '(= 0 1))

        (mk-proof '()
                  '(= 0 0))))

(define category
  (list (#|Left identity|#
         mk-proof '((im 0))
                  '(= (compose 0 1) 0))

        (#|Right identity|#
         mk-proof '((im 0))
                  '(= (compose 1 0) 0))

        (#|The demand for identity morphism|#
         mk-proof '((= (compose 0 (f)) 0)
                    (= (compose (f) 0) 0))
                  '(im 0))))
