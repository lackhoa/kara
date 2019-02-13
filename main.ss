(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")
;; (load "faster-miniKanren/matche.scm")
;; (load "faster-miniKanren/numbers.scm")
;; (load "miniKanren/mk.scm")

(load "reif.ss")

(define pp (lambda (ls) (for-each pretty-print ls)))

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

;; (time
;;  (let loop [(i 100000)]
;;    (unless (= i 0)
;;      (run* (q)
;;        (memberd-impure 'z a-z))
;;      (loop (- i 1)))))

;; (time
;;  (let loop [(i 100000)]
;;    (unless (= i 0)
;;      (run* (q)
;;        (memberd 'z a-z))
;;      (loop (- i 1)))))

;; Tests for the full interpreter
(;; This is my version
 load "full-interp.ss")
;; (;; This is the one in faster-mk
;;  load "faster-miniKanren/full-interp.scm")

;; (time
;;  ;; Something random: runs 4x slower on my interp
;;  (run 500 (_.0 _.1 _.2 _.3)
;;    (evalo
;;     `(match ,_.0 [`,_.0 ,_.1] . ,_.2)
;;     _.1)))

;; Summary of reif interp vs the original
;; lambda: no big differences
;; letrec: 2x faster
;; pattern matching: 2x slower
;; Free-form evalo: 2x faster
;; quote: 2x slower

(time
 (run 1000 (x y)
   ;; (fresh (e)
   ;;   (== x `(letrec . ,e)))
   (evalo x y)))

;; (time
;;  ;; Something really complicated: 1.5x slower
;;  (let loop [(i 100)]
;;    (unless (= i 0)
;;      (equal?
;;       (run 1 (q)
;;         (== q '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))
;;         (evalo
;;          `(letrec ((eval-quasi (lambda (q eval)
;;                                  (match q
;;                                    [(? symbol? x) x]
;;                                    [`() '()]
;;                                    [`(,`unquote ,exp) (eval exp)]
;;                                    [`(quasiquote ,datum) ('error)]
;;                                    [`(,a . ,d)
;;                                     (cons (eval-quasi a eval)
;;                                           (eval-quasi d eval))]))))
;;             (letrec ((eval-expr
;;                       (lambda (expr env)
;;                         (match expr
;;                           [`(quote ,datum) datum]
;;                           [`(lambda (,(? symbol? x)) ,body)
;;                            (lambda (a)
;;                              (eval-expr body (lambda (y)
;;                                                (if (equal? x y)
;;                                                    a
;;                                                    (env y)))))]
;;                           [(? symbol? x) (env x)]
;;                           [`(quasiquote ,datum)
;;                            (eval-quasi datum (lambda (exp) (eval-expr
;;                                                        exp env)))]
;;                           [`(,rator ,rand)
;;                            ((eval-expr rator env) (eval-expr rand env))]
;;                           ))))
;;               (eval-expr ',q
;;                          'initial-env)))
;;          q))
;;       (list '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))))
;;      (loop (- i 1)))))
