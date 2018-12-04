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
   '(;; Leg 4 is mounted at point 0 in circuit 10
     (mnt 4 0 10)
     (mem (0 . 55) 10)
     (mem 4 55))

   '(;; Path going through one device
     (path [4] 0 1 10)
     (mnt (4 8) 0)
     (mnt (4 9) 1)
     (=/= 8 9))

   '(;; Complex path
     (path [(8 9) 2 . 22] 0 1 10)
     (mnt 0 (4 5) 10)
     (;; Goes in on one end of the device -> comes out on the other
      =/= 3 4)
     (point-id 3 10)
     (;; No loops
      !mem (2 3) 22)
     (path 22 2 1 10))))

(define ca40
  '((p1 (r1 a) (t1 a))
    (p2 (r1 b) (r2 a) (r4 a) (r5 a))
    (p3 (r2 b) (r3 a))
    (p4 (r3 b) (r4 b) (r5 b) (t2 a))))

(define ca49
  '((p1 (r1 a) (r3 a) (b +))
    (p2 (r2 a) (r4 a) (b -))
    (p3 (r1 b) (r2 b) (r5 a))
    (p4 (r3 b) (r4 b) (r5 b))))
