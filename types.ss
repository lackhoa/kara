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

(define ls-proof
  (lambda ls
    (map (l> apply mk-proof) ls)))

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
  (ls-proof '((= 0 1)
              (= 1 0))

            '((= 0 2)
              (= 0 1) '(= 1 2))))

(define category
  (ls-proof '((= 1 3)
              (-> 0 1 2)
              (-> 0 3 4))

            '((= 2 4)
              (-> 0 1 2)
              (-> 0 3 4))

            '((= (c (c 0 1) 2)
                 (c 0 (c 1 2)))
              (-> 2 3 4)
              (-> 1 4 5)
              (-> 0 5 6))

            '((= (c 0 (- 0)) 2)
              (i-> 0)
              (-> 0 1 2))

            '((= (c (- 0) 0) 1)
              (i-> 0)
              (-> 0 1 2))))

(define list-axioms
  (ls-proof
   '((mem 0 (0 . 1)))

   '((mem 0 (1 . 2))
     (=/= 0 1)
     (mem 0 2))

   '((!mem 0 ()))

   '((!mem 0 (1 . 2))
     (=/= 0 1)
     (!mem 0 2))))

(define circuit
  (ls-proof
   '(;; Device 4 is mounted at point 0 in circuit 10
     (mnt 4 0 10)
     (mem (point 0 . 55) 10)
     (mem 4 55))

   '(;; Path through one device
     (path [8] 0 1 10)
     (=/= 0 1)
     (mnt 8 0 10) (mnt 8 1 10))

   '(;; Complex path
     (path [8 2 . 22] 0 1 10)
     (=/= 0 2) (=/= 2 1)
     (mnt 8 0 10) (mnt 8 2 10)
     ;; No loops
     (!mem 8 22) (!mem 0 22)
     (path 22 2 1 10))))

(define ca40
  '((point p1 r1 t1)
    (point p2 r1 r2 r4 r5)
    (point p3 r2 r3)
    (point p4 r3 r4 r5 t2)))

(define ca49
  '((bat b p1 p2)
    (point p1 r1 r3 b)
    (point p2 r2 r4 b)
    (point p3 r1 r2 r5)
    (point p4 r3 r4 r5)))
