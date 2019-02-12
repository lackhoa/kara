(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")
(load "faster-miniKanren/matche.scm")
(load "faster-miniKanren/numbers.scm")

(define pp (lambda (ls) (for-each pretty-print ls)))

(load "reif.ss")
;; This is my version
(load "full-interp.ss")

(display
 (equal?
  (run 1 (q)
    (== q '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))
    (evalo
     `(letrec ((eval-quasi (lambda (q eval)
                             (match q
                               [(? symbol? x) x]
                               [`() '()]
                               [`(,`unquote ,exp) (eval exp)]
                               [`(quasiquote ,datum) ('error)]
                               [`(,a . ,d)
                                (cons (eval-quasi a eval)
                                      (eval-quasi d eval))]))))
        (letrec ((eval-expr
                  (lambda (expr env)
                    (match expr
                      [`(quote ,datum) datum]
                      [`(lambda (,(? symbol? x)) ,body)
                       (lambda (a)
                         (eval-expr body (lambda (y)
                                           (if (equal? x y)
                                               a
                                               (env y)))))]
                      [(? symbol? x) (env x)]
                      [`(quasiquote ,datum)
                       (eval-quasi datum (lambda (exp) (eval-expr
                                                   exp env)))]
                      [`(,rator ,rand)
                       ((eval-expr rator env) (eval-expr rand env))]
                      ))))
          (eval-expr ',q
                     'initial-env)))
     q))
  (list '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))))

;; This is the one in faster-mk
(load "faster-miniKanren/full-interp.scm")
