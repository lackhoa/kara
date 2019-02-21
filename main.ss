;;; miniKanren
;; (load "faster-miniKanren/mk-vicare.scm")
;; (load "faster-miniKanren/mk.scm")
;; (load "faster-miniKanren/matche.scm")
;; (load "faster-miniKanren/numbers.scm")
;; (load "miniKanren/mk.scm")
(load "micro.ss")

;;; My stuff
;; (load "compiler.ss")

;;; Help functions
(define pp (lambda (ls) (for-each pretty-print ls)))

(define repeat
  (lambda (i f)
    (unless (= i 0) (f) (repeat (- i 1) f))))

(define reflect
  ;; For debugging
  (lambda (x)
    (project (x)
      (begin (display x) (newline)
             succeed))))

;;; The main program
(defrel (appendo l1 l2 l)
  (conde
   [(== '() l1) (== l2 l)]
   [(fresh (a d l-d)
      (== `(,a . ,d) l1)
      (== `(,a . ,l-d) l)
      (appendo d l2 l-d))]))

(pp
 (run* (q p)
   (appendo q p '(1 2 3 4 5))))
