(import (chezscheme)
        (kara-lang main))
(load "mol.ss")

;;; Utility functions
(define get-ccs
  (f> ref '[cdr car]))

(define get-prem
  (f> ref '[cdr cdr]))

(define fill-underscore
  (lambda (root)
    (let ([get-var  (let ([next  (last-var root)])
                      (lambda _
                        (set! next (1+ next))
                        next))])
      (let loop ([mol  root])
        (mol-< mol
               identity
               (lambda (c)   (if (eq? c '_) (get-var)
                            c))
               (lambda (pr)  (cons (loop (car pr))
                              (loop (cdr pr)))))))))

(define mk-proof
  (lambda (conclusion . premises)
    (>> `(=> ,conclusion
            ,@(let loop ([premises  premises])
                (if (null? premises)  (list)
                    (cons `(=> ,(car premises) . _)
                          (loop (cdr premises))))))
        fill-underscore)))

(define ls-proof
  (lambda ls
    (append (;; The fact that the axiom exists
             map (lambda (x)  `(=> (=> ,@x))) ls)
            (;; The version for deduction
             map (l> apply mk-proof) ls))))

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

(define meta
  (ls-proof '((derive . 0)
              (from () derive . 0))

            '(;; Derivation of the truth
              (from _ derive))
            '(;; Derivation from assumption
              (from 11 derive 1)
              (mem 1 11))
            '(;; Derivation from axiom
              (from 10 derive 0)
              (=> 0 . 1) (from 10 derive . 1))
            '(;; Derivation of many
              (from 2 derive 0 . 1)
              (from 2 derive 0) (from 2 derive . 1))))

(define equality
  (ls-proof '((= 0 1)
              (= 1 0))

            '((= 0 2)
              (= 0 1) (= 1 2))))

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

(define misc
  (ls-proof
   '((and))
   '((and 1 . 10) 1 (and . 10))
   '((or 1 . 10) 1)
   '((or 1 . 10) (or . 10)))
  )

(define apply-axioms
  (ls-proof
   '((apply 3 1 3)
     (atom 3) (=/= 3 *))

   '((apply * 0 0))

   '((apply (1 . 11) 0 (2 . 22))
     (apply 1 0 2) (apply 11 0 22))))

(define list-axioms
  (ls-proof
   '((mem 0 (0 . 1)))
   '((mem 0 (1 . 2))
     (mem 0 2))

   '((append () 0 0))
   '((append (1 . 10) 0 (1 . 2))
     (append 10 0 2))

   '((append-dl (++ 0 1)
                (++ 1 2)
                (++ 0 2)))

   '((last (0) 0))
   '((last (0 . 1) 2)
     (last 1 2))

   '((remove 1 () ()))
   '((remove 1 (1 . 10) 10))
   '((remove 1 (2 . 10) (2 . 9))
     (=/= 1 2) (remove 1 10 9))

   '((map 5 () ()))
   '((map 5 (1 . 10) (2 . 20))
     (apply 5 1 2) (map 5 10 20))

   '((forall 5 ()))
   '((forall 5 (1 . 10))
     (apply 5 1 2) 2 (forall 5 10))

   '((reverse 0 1)
     (reverse 0 () 1))
   '((reverse () 0 0))
   '((reverse (1 . 10) 0 2)
     (reverse 10 (1 . 0) 2))
   ))

(define knowledge
  (ls-proof
   '((known 1)
     (remove 1 11 10) (forall (known *) 10) (related . 11))))

(define circuit
  (ls-proof
   '(;; Ohm's law
     (related . ((v 0 1) (i 0 1) (r 0 1))))

   '(;; Kirchhoff's current law
     (related . 33)
     (all 0 . 88) (map (i *) 88 33))

   '(;; Kirchhoff's voltage law
     (related . 33)
     (path (0 . 22) 0)
     (KVL 0 (0 . 22) 33))

   '(;; KVL processing: last node
     (KVL 0 (1) ((v 1 0))))

   '(;; KVL processing: intermediate nodes
     (KVL 0 (1 2 . 22) ((v 1 2) . 33))
     (KVL 0 (2 . 22) 33))

   '(;; Assumption of resistance
     (known (r 0 1))
     (=/= 0 1) (res 2) (at 0 2) (at 1 2))

   ;; Paths (the last node is left out for a reason)
   '(;; Path through one device
     (path (0) 1)
     (=/= 0 1) (at 0 8) (at 1 8))

   '(;; Complex path
     (path (0 2 . 22) 1)
     (=/= 0 2) (=/= 2 1) (at 0 8) (at 2 8)
     ;; No loops
     (;; note that 0 can be 1
      !mem 0 22)
     (path (2 . 22) 1))
   ))

(define ca40
  '(((res r1) (res r2) (res r3) (res r4) (res r5))

    (all p1 r1 t1)
    (all p2 r1 r2 r4 r5)
    (all p3 r2 r3)
    (all p4 r3 r4 r5 t2)))

(define ca49
  '(((bat b p1 p2)
     (res r1) (res r2) (res r3) (res r4) (res r5))

    (all p1 r1 r3 b)
    (all p2 r2 r4 b)
    (all p3 r1 r2 r5)
    (all p4 r3 r4 r5)))

(define get-mount-points
  (lambda (point-lists)
    (flatmap (lambda (point-list)
               (let* ([point  (cadr point-list)]
                      [devs   (cddr point-list)])
                 (map (lambda (dev)  `(at ,point ,dev))
                      devs)))
             point-lists)))

(define parse-circuit
  (lambda (circ)
    (apply ls-proof
      (>> (append (car circ)
                  (cdr circ)
                  (get-mount-points (cdr circ)))
          (l> map list)))))

(define default
  (ls-proof '((default (:- (flies 0) (bird 0))))
            '((rule (:- (not (flies 0)) (penguin 0))))
            '((rule (:- (bird 0) (penguin 0))))
            '((rule (:- (penguin tweety))))
            '((rule (:- (bird opus))))

            '((explain 0 1)
              (explain 0 () 1))
            '((explain () 0 0))
            '((explain (0 . 1) 2)
              (explain ))))
