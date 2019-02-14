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

(define-syntax ifa
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((c-inf e))
       (case-inf c-inf
                 (() (ifa b ...))
                 ((f) (inc (loop (f))))
                 ((a) (bind* c-inf g ...))
                 ((a f) (bind* c-inf g ...)))))))
(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (c)
       (inc
        (ifa ((g0 c) g ...)
             ((g1 c) g^ ...) ...))))))
(define-syntax ifu
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((c-inf e))
       (case-inf c-inf
                 (() (ifu b ...))
                 ((f) (inc (loop (f))))
                 ((c) (bind* c-inf g ...))
                 ((c f) (bind* (unit c) g ...)))))))
(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (c)
       (inc
        (ifu ((g0 c) g ...)
             ((g1 c) g^ ...) ...))))))

;; Test for the reif functionalities
(define a-z
  '(a b c d e f g h i j k l m n o p q r s t u v w z y z))

;; (time (repeat 100000 (run* (q) (memberd-impure 'z a-z))))
;; (time (repeat 100000 (run* (q) (memberd 'z a-z))))

;; Tests for the full interpreter
;; (;; This is my version
;;  load "full-interp.ss")

;; (;; This is the one in faster-mk
;;  load "faster-miniKanren/full-interp.scm")

(;; This is the test version, when I mix things in between
 load "test-interp.ss")

(pp
 (run 4 (x y)
   (fresh (_.0 _.1 _.2)
     (== x y)
     (evalo x y))))

;; (time
;;  ;; Something random: 4x slower
;;  (run 500 (_.0 _.1 _.2 _.3)
;;    (evalo
;;     `(match ,_.0 [`,_.0 ,_.1] . ,_.2)
;;     _.1)))

;; Summary of reif interp vs the original
;; quote: no big difference (there's only one answer, though)
;; lambda: no big difference
;; letrec: 2x faster
;; pattern matching: 2x slower
;; Free-form evalo: 2x faster
;; Quine: can't do anything 'bout it (unless with minor hinting, still slower)



;; (time
;;  ;; Something really complicated: 1.5x slower
;;  (repeat 1000
;;          (equal?
;;           (run 1 (q)
;;             (== q '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))
;;             (evalo
;;              `(letrec ((eval-quasi (lambda (q eval)
;;                                      (match q
;;                                        [(? symbol? x) x]
;;                                        [`() '()]
;;                                        [`(,`unquote ,exp) (eval exp)]
;;                                        [`(quasiquote ,datum) ('error)]
;;                                        [`(,a . ,d)
;;                                         (cons (eval-quasi a eval)
;;                                               (eval-quasi d eval))]))))
;;                 (letrec ((eval-expr
;;                           (lambda (expr env)
;;                             (match expr
;;                               [`(quote ,datum) datum]
;;                               [`(lambda (,(? symbol? x)) ,body)
;;                                (lambda (a)
;;                                  (eval-expr body (lambda (y)
;;                                                    (if (equal? x y)
;;                                                        a
;;                                                        (env y)))))]
;;                               [(? symbol? x) (env x)]
;;                               [`(quasiquote ,datum)
;;                                (eval-quasi datum (lambda (exp) (eval-expr
;;                                                            exp env)))]
;;                               [`(,rator ,rand)
;;                                ((eval-expr rator env) (eval-expr rand env))]
;;                               ))))
;;                   (eval-expr ',q
;;                              'initial-env)))
;;              q))
;;           (list '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))))))
