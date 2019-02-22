;;; miniKanren
(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")
;; (load "faster-miniKanren/matche.scm")
;; (load "faster-miniKanren/numbers.scm")
;; (load "miniKanren/mk.scm")
;; (load "micro.ss")

;;; Loading my stuff
;; (load "compiler.ss")
(load "exp-interp.ss")

;;; Help functions
(define pp (lambda (ls) (for-each pretty-print ls)))

(define repeat-func
  (lambda (i f)
    (unless (= i 0) (f) (repeat (- i 1) f))))
(define-syntax repeat
  (syntax-rules ()
    [(_ i e)
     (repeat-func i (lambda () e))]))

(define reflect
  ;; For debugging
  (lambda (x)
    (project (x)
      (begin (display x) (newline)
             succeed))))

;;; The main program
(define proof?-evalo
  (lambda (proof result)
    (evalo
     `(letrec ([member?
                (lambda (x ls)
                  (match ls
                    [`() #f]
                    [`(,a . ,d)
                     (or (equal? a x) (member? x d))]))])
        (letrec ([proof?
                  (lambda (proof)
                    (match proof
                      [`(,A ,assms assumption ())
                       (member? A assms)]
                      [`(,B ,assms modus-ponens
                            (((,A => ,B) ,assms ,r1 ,ants1)
                             (,A ,assms ,r2 ,ants2)))
                       (and (proof? (list (list A '=> B) assms r1 ants1))
                          (proof? (list A assms r2 ants2)))]
                      [`((,A => ,B) ,assms conditional
                         ((,B (,A . ,assms) ,rule ,ants)))
                       (proof? (list B (cons A assms) rule ants))]))])
          (proof? ',proof)))
     result)))

(define example-proof
  ;; prove C holds, given A, A => B, B => C
  '(C (A (A => B) (B => C))
      modus-ponens
      (((B => C) (A (A => B) (B => C)) assumption ())
       (B (A (A => B) (B => C))
          modus-ponens
          (((A => B) (A (A => B) (B => C)) assumption ())
           (A (A (A => B) (B => C)) assumption ()))))))

(pp
 (run 999 (q)
   (evalo '(cdr (lambda (x) x)) q)))
