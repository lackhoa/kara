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
     (=/= 0 1) (mem 0 2))

   '((!mem 0 ()))

   '((!mem 0 (1 . 2))
     (=/= 0 1) (!mem 0 2))

   '((forall 5 ()))
   '((forall 5 (1 . 11))
     (apply 5 1 2) 2 (forall 5 11))))

(define circuit
  (ls-proof
   '(;; Path through one device
     (path [8] 0 1)
     (=/= 0 1)
     (at 0 8) (at 1 8))

   '(;; Complex path
     (path [8 2 . 22] 0 1)
     (=/= 0 2) (=/= 2 1)
     (at 0 8) (at 2 8)
     ;; No loops
     (!mem 8 22) (!mem 0 22)
     (path 22 2 1))

   '(;; Series-parallel circuit: A single resistor
     (sp 8 0 1)
     (res 8) (at 0 8) (at 1 8) (=/= 0 1))

   '(;; Part of an sp circuit: resistor
     (sp-part 8 (sp 8 _ _)))

   '(;; Part of an sp circuit: series
     (sp-part 7 (sr 8 9))
     (sp-part 7 8))
   '((sp-part 7 (sr 8 9))
     (sp-part 7 9))

   '(;; Part of an sp circuit: parallel
     (sp-part 7 (pr 8 9))
     (sp-part 7 8))
   '((sp-part 7 (pr 8 9))
     (sp-part 7 9))

   '(;; Sp circuit: a series
     (sp (sr 8 9) 0 1)
     (=/= 0 1) (sp 8 0 2) (sp 8 2 1)
     ;; All devices at 2 must be a part of 8 or 9
     (all 2 . 22)
     (forall (sp-part * 8) 22)
     (forall (sp-part * 9) 22))

   '(;; Sp circuit: a parallel
     (sp (pr 8 9) 0 1)
     (=/= 0 1) (=/= 8 9) (sp 8 0 1) (sp 9 0 1))
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
