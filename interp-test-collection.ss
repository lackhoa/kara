;; Summary of reif interp vs the original
;; quote: no big difference (there's only one answer, though)
;; lambda: no big difference
;; letrec: 2x faster
;; pattern matching: 2x slower
;; Free-form evalo: 2x faster
;; Quine: can't do anything 'bout it (unless with minor hinting, still slower)


(time
 (run 100 (x y)
   (fresh (_.0 _.1 _.2)
     (== x
        `(letrec ([even-list?
                   (lambda (ls)
                     (match ls
                       [`() #t]
                       [`(,a ,b . ,d) (even-list? d)]
                       [else #f]))])
           (even-list? ,_.0)))
     (== y #t)
     (evalo x y))))


(time
 ;; Quine with quasi-quotation: 1.3x slower
 (equal?
  (repeat 500
          (lambda ()
            (run 1 (q)
              (== q '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))
              (evalo
               `(letrec ((eval-quasi
                          (lambda (q eval)
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
                                 ((eval-expr rator env) (eval-expr rand env))]))))
                    (eval-expr ',q
                               'initial-env)))
               q))))
  (list '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))))
