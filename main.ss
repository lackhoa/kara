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

;;; miniKanren
(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")
;; (load "faster-miniKanren/matche.scm")
;; (load "faster-miniKanren/numbers.scm")
;; (load "miniKanren/mk.scm")
;; (load "micro.ss")

;;; Other "libraries"
;; (load "faster-miniKanren/full-interp.scm")
(load "reif.ss")

;; The main program
;; (load "full-interp.ss")
;; (load "test-reif.ss")
;; (load "test-full-interp.ss")
;; (load "compiler.ss") ;; (load "test-compiler.ss")
(load "net.ss") (load "test-net.ss")
