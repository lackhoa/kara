(time
 (equal?
  (length
   (run 5 (x*)
     (evalo `(letrec ([even-list?
                       (lambda (ls)
                         (match ls
                           [`() #t]
                           [`(,a ,b . ,d) (even-list? d)]
                           [else #f]))])
               (even-list? ',x*))
            #t)))
  5))
(newline)

(time
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
  (list '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))))
(newline)

(define proof?-evalo
  (lambda (proof result)
    (evalo
     `(letrec ([member?
                (lambda (x ls)
                  (match ls
                    [`() #f]
                    [`(,a . ,d)
                     (or (equal? a x) (member? x d))]))])
        (letrec ([proof?
                  (lambda (proof)
                    (match proof
                      [`(,A ,assms assumption ())
                       (member? A assms)]
                      [`(,B ,assms modus-ponens
                            (((,A => ,B) ,assms ,r1 ,ants1)
                             (,A ,assms ,r2 ,ants2)))
                       (and (proof? (list (list A '=> B) assms r1 ants1))
                          (proof? (list A assms r2 ants2)))]
                      [`((,A => ,B) ,assms conditional
                         ((,B (,A . ,assms) ,rule ,ants)))
                       (proof? (list B (cons A assms) rule ants))]))])
          (proof? ',proof)))
     result)))

(define example-proof
  ;; prove C holds, given A, A => B, B => C
  '(C (A (A => B) (B => C))
      modus-ponens
      (((B => C) (A (A => B) (B => C)) assumption ())
       (B (A (A => B) (B => C))
          modus-ponens
          (((A => B) (A (A => B) (B => C)) assumption ())
           (A (A (A => B) (B => C)) assumption ()))))))

(time
 (run 1 (q)
   (proof?-evalo example-proof q)))

(display "Takes a few seconds...")
(newline)
(time
 (run 4 (q)
   (evalo q q)))
