;; The definition of 'letrec' is based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.

(define evalo
  (lambda (expr val)
    (eval-expo expr initial-env val)))

(define eval-expo
  (lambda (expr env val)
    (conde [(== `(quote ,val) expr)
            (absento 'closure val)
            (absento 'prim val)
            (lookupo 'quote env 'unbound #f)]

           [(numbero expr) (== expr val)]

           [(symbolo expr) (lookupo expr env val #t)]

           [(fresh (x body)
              (== `(lambda ,x ,body) expr)
              (== `(closure (lambda ,x ,body) ,env) val)
              (conde [;; Variadic
                      (symbolo x)]
                     [;; Multi-argument
                      (list-of-symbolso x)])
              (lookupo 'lambda env 'unbound #f))]

           [(fresh (rator x rands body env^ a* res)
              (== `(,rator . ,rands) expr)
              ;; variadic
              (symbolo x)
              (== `((,x . (val . ,a*)) . ,env^) res)
              (eval-expo rator env `(closure (lambda ,x ,body) ,env^))
              (eval-expo body res val)
              (eval-listo rands env a*))]

           [(fresh (rator x* rands body env^ a* res)
              (== `(,rator . ,rands) expr)
              ;; Multi-argument
              (eval-expo rator env `(closure (lambda ,x* ,body) ,env^))
              (eval-listo rands env a*)
              (ext-env*o x* a* env^ res)
              (eval-expo body res val))]

           [(fresh (rator x* rands a* prim-id)
              (== `(,rator . ,rands) expr)
              (eval-expo rator env `(prim . ,prim-id))
              (eval-primo prim-id a* val)
              (eval-listo rands env a*))]

           [(handle-matcho expr env val)]

           [(fresh (p-name x body letrec-body)
              ;; single-function variadic letrec version
              (== `(letrec ((,p-name (lambda ,x ,body)))
                    ,letrec-body)
                 expr)
              (conde [;; Variadic
                      (symbolo x)]
                     [;; Multiple argument
                      (list-of-symbolso x)])
              (lookupo 'letrec env 'unbound #f)
              (eval-expo letrec-body
                         `((,p-name . (rec . (lambda ,x ,body))) . ,env)
                         val))]

           [(prim-expo expr env val)])))

(define empty-env '())

(define lookupo
  ;; bound? tells whether the variable is bound
  (lambda (x env t bound?)
    (conde [(== bound? #f)
            (== t 'unbound)
            (== empty-env env)]
           [(fresh (y b rest)
              (== `((,y . ,b) . ,rest) env)
              (conde [(== bound? #t)
                      (== x y)
                      (conde [(== `(val . ,t) b)]
                             [(fresh (lam-expr)
                                (== `(rec . ,lam-expr) b)
                                (== `(closure ,lam-expr ,env) t))])]
                     [(=/= x y)
                      (lookupo x rest t bound?)]))])))

(define eval-listo
  (lambda (expr env val)
    (conde [(== '() expr)
            (== '() val)]
           [(fresh (a d v-a v-d)
              (== `(,a . ,d) expr)
              (== `(,v-a . ,v-d) val)
              (eval-expo a env v-a)
              (eval-listo d env v-d))])))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(define list-of-symbolso
  (lambda (los)
    (conde [(== '() los)]
           [(fresh (a d)
              (== `(,a . ,d) los)
              (symbolo a)
              (list-of-symbolso d))])))

(define ext-env*o
  (lambda (x* a* env out)
    (conde
     [(== '() x*) (== '() a*) (== env out)]
     [(fresh (x a dx* da* env2)
        (== `(,x . ,dx*) x*)
        (== `(,a . ,da*) a*)
        (== `((,x . (val . ,a)) . ,env) env2)
        (symbolo x)
        (ext-env*o dx* da* env2 out))])))

(define eval-primo
  (lambda (prim-id a* val)
    (conde [(== prim-id 'cons)
            (fresh (a d)
              (== `(,a ,d) a*)
              (== `(,a . ,d) val))]
           [(== prim-id 'car)
            (fresh (d)
              (== `((,val . ,d)) a*)
              (=/= 'closure val))]
           [(== prim-id 'cdr)
            (fresh (a)
              (== `((,a . ,val)) a*)
              (=/= 'closure a))]
           [(== prim-id 'not)
            (fresh (b)
              (== `(,b) a*)
              (conde ((=/= #f b) (== #f val))
                     ((== #f b) (== #t val))))]
           [(== prim-id 'equal?)
            (fresh (v1 v2)
              (== `(,v1 ,v2) a*)
              (conde [(== v1 v2) (== #t val)]
                     [(=/= v1 v2) (== #f val)]))]
           [(== prim-id 'symbol?)
            (fresh (v sort)
              (== `(,v) a*)
              (conde [(== sort 'symbol) (== #t val)]
                     [(=/= sort 'symbol) (== #f val)])
              (exp-sorto v sort))]
           [(== prim-id 'null?)
            (fresh (v)
              (== `(,v) a*)
              (conde [(== '() v) (== #t val)]
                     [(=/= '() v) (== #f val)]))])))

(define prim-expo
  (lambda (expr env val)
    (conde [(boolean-primo expr env val)]
           [(and-primo     expr env val)]
           [(or-primo      expr env val)]
           [(if-primo      expr env val)])))

(define boolean-primo
  (lambda (expr env val)
    (conde [(== #t expr) (== #t val)]
           [(== #f expr) (== #f val)])))

(define and-primo
  (lambda (expr env val)
    (fresh (e*)
      (== `(and . ,e*) expr)
      (lookupo 'and env 'unbound #f)
      (ando e* env val))))

(define ando
  (lambda (e* env val)
    (conde
     [(== '() e*) (== #t val)]
     [(fresh (e)
        (== `(,e) e*)
        (eval-expo e env val))]
     [(fresh (e1 e2 e-rest v)
        (== `(,e1 ,e2 . ,e-rest) e*)
        (conde [(== #f v)
                (== #f val)
                (eval-expo e1 env v)]
               [(=/= #f v)
                (eval-expo e1 env v)
                (ando `(,e2 . ,e-rest) env val)]))])))

(define or-primo
  (lambda (expr env val)
    (fresh (e*)
      (== `(or . ,e*) expr)
      (lookupo 'or env 'unbound #f)
      (oro e* env val))))

(define oro
  (lambda (e* env val)
    (conde [(== '() e*) (== #f val)]
           [(fresh (e)
              (== `(,e) e*)
              (eval-expo e env val))]
           [(fresh (e1 e2 e-rest v)
              (== `(,e1 ,e2 . ,e-rest) e*)
              (conde [(=/= #f v)
                      (== v val)
                      (eval-expo e1 env v)]
                     [(== #f v)
                      (eval-expo e1 env v)
                      (oro `(,e2 . ,e-rest) env val)]))])))

(define if-primo
  (lambda (expr env val)
    (fresh (e1 e2 e3 t)
      (== `(if ,e1 ,e2 ,e3) expr)
      (lookupo 'if env 'unbound #f)
      (eval-expo e1 env t)
      (conde [(=/= #f t) (eval-expo e2 env val)]
             [(== #f t) (eval-expo e3 env val)]))))

(define initial-env `((list    . (val . (closure (lambda x x) ,empty-env)))
                      (not       . (val . (prim . not)))
                      (equal?  . (val . (prim . equal?)))
                      (symbol? . (val . (prim . symbol?)))
                      (cons    . (val . (prim . cons)))
                      (null?   . (val . (prim . null?)))
                      (car     . (val . (prim . car)))
                      (cdr     . (val . (prim . cdr)))
                      . ,empty-env))

;;; Pattern matching business

(define handle-matcho
  (lambda (expr env val)
    (fresh (against-expr mval clause clauses)
      (== `(match ,against-expr ,clause . ,clauses) expr)
      (lookupo 'match env 'unbound #f)
      (eval-expo against-expr env mval)
      (match-clauses mval `(,clause . ,clauses) env val))))

;; To fix not-symbolo and not-numbero
(define exp-sorto
  (lambda (t sort)
    (conde [(== sort 'symbol)  (symbolo t)]
           [(== sort 'boolean) (conde [(== t #t)] [(== t #f)])]
           [(== sort 'number)  (numbero t)]
           [(== sort 'null)    (== '() t)]
           [(== sort 'pair)    (fresh (a d)
                                (== `(,a . ,d) t))])))

(define not-symbolo
  (lambda (t)
    (fresh (sort)
      (=/= sort 'symbol)
      (exp-sorto t sort))))

(define not-numbero
  (lambda (t)
    (fresh (sort)
      (=/= sort 'number)
      (exp-sorto t sort))))

(define self-eval-literalo
  (lambda (t)
    (conde [(numbero t)]
           [(booleano t)])))

(define literalo
  (lambda (t)
    (conde [(numbero t)]
           [(symbolo t) (=/= 'closure t)]
           [(booleano t)]
           [(== '() t)])))

(define booleano
  (lambda (t)
    (conde [(== #f t)]
           [(== #t t)])))

(define regular-env-appendo
  (lambda (env1 env2 env-out)
    (conde [(== empty-env env1) (== env2 env-out)]
           [(fresh (y v rest res)
              (== `((,y . (val . ,v)) . ,rest) env1)
              (== `((,y . (val . ,v)) . ,res) env-out)
              (regular-env-appendo rest env2 res))])))

(define match-clauses
  (lambda (mval clauses env val)
    (fresh (p result-expr d penv)
      (== `((,p ,result-expr) . ,d) clauses)
      (conde [(fresh (env^)
                (p-match p mval '() penv #t)
                (regular-env-appendo penv env env^)
                (eval-expo result-expr env^ val))]
             [(p-match p mval '() penv #f)
              (match-clauses mval d env val)]))))

(define var-p-match
  (lambda (var mval penv penv-out match?)
    (fresh (val)
      (symbolo var)
      (=/= 'closure mval)
      (conde [(== match? #t)
              (== mval val)
              (== penv penv-out)
              (lookupo var penv val #t)]
             [(== match? #t)
              (== `((,var . (val . ,mval)) . ,penv) penv-out)
              (lookupo var penv 'unbound #f)]
             [(== match? #f)
              (=/= mval val)
              (== penv penv-out)
              (lookupo var penv val #t)]))))

(define p-match
  (lambda (p mval penv penv-out match?)
    (conde [(self-eval-literalo p)
            (== penv penv-out)
            (conde [(== match? #t) (== p mval)]
                   [(== match? #f) (=/= p mval)])]
           [(== match? #t)
            (var-p-match p mval penv penv-out #t)]
           [(== match? #f)
            (var-p-match p mval penv penv-out #f)]
           [(fresh (var pred val)
              (== `(? ,pred ,var) p)
              (conde [(== match? #t)
                      (conde [(== 'symbol? pred) (symbolo mval)]
                             [(== 'number? pred) (numbero mval)])
                      (var-p-match var mval penv penv-out #t)]
                     [(== match? #f)
                      (conde [(== 'symbol? pred)
                              (conde [(not-symbolo mval)]
                                     [(symbolo mval)
                                      (var-p-match var mval penv penv-out #f)])]
                             [(== 'number? pred)
                              (conde [(not-numbero mval)]
                                     [(numbero mval)
                                      (var-p-match var mval penv penv-out #f)])]
                             [(== penv penv-out)
                              (symbolo var)])]))]
           [(fresh (quasi-p)
              (== (list 'quasiquote quasi-p) p)
              (conde [(== match #t)
                      (quasi-p-match quasi-p mval penv penv-out #t)]
                     [(quasi-p-match quasi-p mval penv penv-out #f)]))])))

(define quasi-p-match
  (lambda (quasi-p mval penv penv-out match?)
    (conde [(== penv penv-out)
            (literalo quasi-p)
            (conde [(== match? #t) (== quasi-p mval)]
                   [(== match? #f) (=/= quasi-p mval)])]
           [(fresh (p)
              (== (list 'unquote p) quasi-p)
              (conde [(== match? #t)
                      (p-match p mval penv penv-out #t)]
                     [(== match? #f)
                      (=/= 'closure mval)
                      (p-match p mval penv penv-out #f)]))]
           [(fresh (a d)
              (== `(,a . ,d) quasi-p)
              (=/= 'unquote a)
              (conde [(== match #f)
                      (== penv penv-out)
                      (literalo mval)]
                     [(fresh (v1 v2 penv^)
                        (== `(,v1 . ,v2) mval)
                        (conde [(== match #t)
                                (quasi-p-match a v1 penv penv^ #t)
                                (quasi-p-match d v2 penv^ penv-out #t)]
                               [(== match #f)
                                (quasi-p-match a v1 penv penv^ #f)]
                               [(== match #f)
                                (quasi-p-match a v1 penv penv^ #t)
                                (quasi-p-match d v2 penv^ penv-out #f)]))]))])))
