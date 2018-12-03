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

(define circuit
  (ls-proof '((res 0 1 2)
              (res 0 2 1))

            '((bat (- 0) 2 1)
              (bat 0 1 2)
              (;; meta-logical constraint
               !neg 0))
            '((bat 0 2 1)
              (bat (- 0) 1 2))

            '((any-elm 0 1 2)
              (res 0 1 2))
            '((any-elm 0 1 2)
              (bat 0 1 2))

            '((path (0) 1 2 3)
              (any-elm 0 1 2)
              (!mem 0 3))

            '((path (0 4 . 1) 2 3 5)
              (any-elm 0 2 4)
              (!mem 4 5)
              (!mem 0 5)
              (path 1 4 3 (0 2 . 5)))

            '((sp 0 1 2)
              (res 0 1 2))

            '((sp (sr 0 1) 2 4)
              (sp 0 2 3)
              (sp 1 3 4)
              (;; Nothing connected to b
               all-elm 3))

            '((sp (pr 0 1) 2 3)
              (sp 0 2 3)
              (sp 1 2 3)
              (=/= 0 1))))

(define ca49
  (ls-proof '((all-elm p1 r1 r3 b1))
            '((all-elm p2 r1 r4 b1))
            '((all-elm p3 r1 r2 r4))
            '((all-elm p4 r3 r4 r5))

            '((res r1 p1 p3))
            '((res r2 p2 p3))
            '((res r3 p1 p4))
            '((res r4 p2 p4))
            '((res r5 p3 p4))
            '((bat b1 p1 p2))))

(define ca40
  (ls-proof '((res r1 p1 p2))
            '((res r2 p2 p3))
            '((res r3 p3 p4))
            '((res r4 p2 p4))
            '((res r5 p2 p4))
            '((ter t1 p1))
            '((ter t2 p4))

            '((all-elm p1 r1 t1))
            '((all-elm p2 r1 r2 r4 r5))
            '((all-elm p3 r2 r3))
            '((all-elm p4 r3 r4 r5 t2))))
