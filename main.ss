(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")
;; (load "faster-miniKanren/matche.scm")
;; (load "faster-miniKanren/numbers.scm")
;; (load "miniKanren/mk.scm")

(load "reif.ss")

(define pp (lambda (ls) (for-each pretty-print ls)))

(define repeat
  (lambda (i f)
    (unless (= i 0) (f) (repeat (- i 1) f))))

(define reflect
  (lambda (x)
    (project (x)
      (begin (display x) (newline)
             succeed))))

;; Tests for the full interpreter
(;; This is my version
 load "full-interp.ss")

;; (;; This is the one in faster-mk
;;  load "faster-miniKanren/full-interp.scm")

(display
 (run* (x y)
   (evalo y #f)))
