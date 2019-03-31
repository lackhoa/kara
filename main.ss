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
      (begin (display x) (newline) succeed))))

;;; miniKanren
;; (load "faster-miniKanren/mk-vicare.scm")
;; (load "faster-miniKanren/mk.scm")
;; (load "faster-miniKanren/matche.scm")
;; (load "faster-miniKanren/numbers.scm")
(load "miniKanren/mk.scm")
;; (load "micro.ss")

;;; Other "libraries"
(load "reif.ss")
(load "pmatch.scm")
(load "test-fw.ss")

;;; The main program
;; (load "faster-miniKanren/full-interp.scm")
;; (load "full-interp.ss")
;; (load "test-reif.ss")
;; (load "test-full-interp.ss")
;; (load "compiler.ss") (load "reif.ss") (load "test-compiler.ss")
;; (load "net.ss") (load "test-net.ss")
(load "coq.ss")
;; (load "lambda.ss")
;; (load "lambda2.ss")

(pp
 (run 1 (q)
   (fresh (base step)
     (indo base step plus0r)
     ;; goal: base; step
     (fresh (X) (subo X 0 plus0 base))
     ;; goal: step
     (fresh (ih ig P P^ P^^)
       (== `(-> ,ih ,ig) step)
       ;; assumption: ih; goal: ig
       (mpo S-eq ih P)
       ;; assumption: ih, P; goal: ig
       (fresh (plus-Sr)
         (mpo (copy =sym) plus-S plus-Sr)
         (mpo eql plus-Sr P^))
       ;; assumption: ih, P, P^; goal: ig
       (mpo P^ P P^^)
       ;; assumption: ih, P, P^, P^^; goal: ig
       (mpo (copy =sym) P^^ ig)))))

#!eof
