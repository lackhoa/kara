;;; miniKanren
;; (load "faster-miniKanren/mk-vicare.scm")
;; (load "faster-miniKanren/mk.scm")
;; (load "faster-miniKanren/matche.scm")
;; (load "faster-miniKanren/numbers.scm")
;; (load "miniKanren/mk.scm")
;; (load "micro.ss")

;;; Loading my stuff
(load "compiler.ss")
;; (load "exp-interp.ss")

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
(display
 (let ([x (var 'x)]
       [y (var 'y)]
       [z (var 'z)])
   ((=/= x z)
    (car ((== y x)
          (car ((== #t x) empty-c)))))))
