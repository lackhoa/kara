;;; miniKanren
;; (load "faster-miniKanren/mk-vicare.scm")
;; (load "faster-miniKanren/mk.scm")
;; (load "faster-miniKanren/matche.scm")
;; (load "faster-miniKanren/numbers.scm")
;; (load "miniKanren/mk.scm")
;; (load "micro.ss")

;;; Loading my stuff
(load "compiler.ss")
;; (load "faster-miniKanren/full-interp.scm")
;; (load "exp-interp.ss")

;;; Help functions
(define pp (lambda (ls) (for-each pretty-print ls)))

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
(load "test-compiler.ss")
;; (load "test-full-interp.ss")
