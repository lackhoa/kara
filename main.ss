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
;; (load "faster-miniKanren/mk-vicare.scm")
;; (load "faster-miniKanren/mk.scm")
;; (load "faster-miniKanren/matche.scm")
;; (load "faster-miniKanren/numbers.scm")
;; (load "miniKanren/mk.scm")
;; (load "micro.ss")

;;; Other "libraries"
;; (load "faster-miniKanren/full-interp.scm")
;; (load "reif.ss")
(load "pmatch.scm")
(load "test-fw.ss")

;;; The main program
;; (load "full-interp.ss")
;; (load "test-reif.ss")
;; (load "test-full-interp.ss")
;; (load "compiler.ss") (load "reif.ss") (load "test-compiler.ss")
;; (load "net.ss") (load "test-net.ss")
;; (load "coq.ss") (load "test-coq.ss")
(load "lambda.ss")
;; (load "lambda2.ss")

;;; Tracing
;; (trace val read-back)

(pp (check-program '()
                   '((define three
                       (the Nat
                            (add1 (add1 (add1 zero)))))
                     (define +
                       (the (-> Nat (-> Nat Nat))
                            (lambda (n)
                              (lambda (k)
                                (rec Nat n
                                     k
                                     (lambda (pred)
                                       (lambda (almost-sum)
                                         (add1 almost-sum))))))))
                     (+ three)
                     ((+ three) three))))
