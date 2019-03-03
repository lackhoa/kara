;;; miniKanren
(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")
;; (load "faster-miniKanren/matche.scm")
;; (load "faster-miniKanren/numbers.scm")
;; (load "miniKanren/mk.scm")
;; (load "micro.ss")

;;; Loading my stuff
;; (load "compiler.ss")
(load "faster-miniKanren/full-interp.scm")
;; (load "exp-interp.ss")

;;; Help functions
(define pp pretty-print)
(define ppl (lambda (ls) (for-each pp ls)))

(define repeat-func
  (lambda (i f)
    (unless (= i 0) (f) (repeat-func (- i 1) f))))
(define-syntax repeat
  (syntax-rules ()
    [(_ i e)
     (repeat-func i (lambda () e))]))

(define reflect
  ;; Goal for debugging
  (lambda (x)
    (project (x)
      (begin (display x) (newline)
             succeed))))

;; The main program
;; (load "test-full-interp.ss")
;; (load "test-compiler.ss")
(pp
 (run* (q)
   (evalo
    '(letrec ([e? (lambda (ls)
                    (match ls
                      [`() #t]
                      [`(,a . ,d) (o? d)]))])
       (letrec ([o? (lambda (ls)
                      (match ls
                        [`() #f]
                        [`(,a . ,d) (e? d)]))])
         (e? '(1 2))))
    q)))
