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

(define substitution
  (list (#|ap-core: ignore binders|#
         bind (exp arg res)
          (mk-proof `(ap-core (bind ,exp) ,arg
                              (bind ,res))
                    `(ap-core ,exp ,arg
                              ,res)))

        (;; ap-core: substitute argument for one star
         mk-proof `(ap-core (*) 0
                            (const 0  #|note the tag|#)))

        (;; ap-core: decrement for two or more stars
         bind (arg stars)
          (mk-proof `(ap-core (* * . ,stars) ,arg
                              (* . ,stars))))

        (;; ap-core: ignore constants
         bind (arg exp)
          (mk-proof `(ap-core (const ,exp) ,arg
                              (const ,exp))))

        (;; ap-core: recursively substitute in lists
         bind (arg ecar ecdr ap-car ap-cdr)
          (mk-proof `(ap-core (:: ,ecar ,ecdr) ,arg
                              (:: ,ap-car ,ap-cdr))

                    `(ap-core ,ecar ,arg
                              ,ap-car)

                    `(ap-core ,ecdr ,arg
                              ,ap-cdr)))

        (;; Starting: remove the first binder
         bind (exp arg res)
          (mk-proof `(apply (bind ,exp) ,arg
                            ,res)
                    `(ap-core ,exp ,arg
                              ,res)))

        (;; Decode constants
         mk-proof `(decode (const 0)
                           0))

        (;; Decode pairs
         bind (ecar ecdr dcar dcdr)
          (mk-proof `(decode (:: ,ecar ,ecdr)
                             (,dcar . ,dcdr))
                    `(decode ,ecar ,dcar)
                    `(decode ,ecdr ,dcdr)))

        (;; Eliminate binders by applying a new varirable
         bind (exp res new-var tmp)
          (mk-proof `(decode (bind ,exp)
                             ,res)
                    `(ap-core ,exp ,new-var
                              ,tmp)
                    `(decode ,tmp ,res)))
        ))
