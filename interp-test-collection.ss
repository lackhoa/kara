(equal?
 (run* (q)
   (let ([e '((lambda (x . y) (cons x y)) 'x 'y 'z)])
     (evalo e q)))
 '((x y z)))

(run 2 (x*)
  (evalo `(letrec ([even-list?
                    (lambda (ls)
                      (match ls
                        [`() #t]
                        [`(,a ,b . ,d) (even-list? d)]
                        [else #f]))])
            (even-list? ,x*))
         #t))

(equal?
 (run* (q)
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
         (eval-expr ',q 'initial-env)))
    q))
 (list '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))))
