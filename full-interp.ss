;; The definition of 'letrec' is based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.

(define evalo
  (lambda (expr val)
    (eval-expo expr initial-env val)))

(define eval-expo
  (lambda (expr env val)
    (conde
     [(== `(quote ,val) expr)
      (absento 'closure val)
      (absento 'prim val)
      ((lookupt 'quote env 'not-bound) #f)]

     [(numbero expr) (== expr val)]

     [(symbolo expr) ((lookupt expr env val) #t)]

     [(fresh (x body)
        (== `(lambda ,x ,body) expr)
        (== `(closure (lambda ,x ,body) ,env) val)
        (conde [;; Variadic
                (symbolo x)]
               [;; Multi-argument
                (list-of-symbolso x)])
        ((lookupt 'lambda env 'not-bound) #f))]

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
        ((lookupt 'letrec env 'not-bound) #f)
        (eval-expo letrec-body
                   `((,p-name . (rec . (lambda ,x ,body))) . ,env)
                   val))]

     [(prim-expo expr env val)])))

(define empty-env '())

(define lookupt
  (lambda (x env t)
    (lambda (bound?)
      (condo [(==t empty-env env)
              (== bound? #f)
              (== t 'not-bound)]
             [else
              (fresh (y b rest)
                (== `((,y . ,b) . ,rest) env)
                (condo [(==t x y)
                        (== bound? #t)
                        (conde [(== `(val . ,t) b)]
                               [(fresh (lam-expr)
                                  (== `(rec . ,lam-expr) b)
                                  (== `(closure ,lam-expr ,env) t))])]
                       [else
                        ((lookupt x rest t) bound?)]))]))))

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
              (typeo v sort))]
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
      ((lookupt 'and env 'not-bound) #f)
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
      ((lookupt 'or env 'not-bound) #f)
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
      ((lookupt 'if env 'not-bound) #f)
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
      ((lookupt 'match env 'not-bound) #f)
      (eval-expo against-expr env mval)
      (match-clauses mval `(,clause . ,clauses) env val))))

(define typeo
  ;; Works in this context at least
  (lambda (term type)
    (conde [(== type 'symbol)  (symbolo term)]
           [(== type 'boolean) (booleano term)]
           [(== type 'number)  (numbero term)]
           [(== type 'null)    (== '() term)]
           [(== type 'pair)    (fresh (a d) (== `(,a . ,d) term))])))

(define typet
  (lambda (term type)
    (lambda (bool)
      (fresh (T)
        ((==t type T) bool)
        (typeo term T)))))

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
      (condo [(p-matcht p mval '() penv)
              (fresh (env^)
                (regular-env-appendo penv env env^)
                (eval-expo result-expr env^ val))]
             [else
              (match-clauses mval d env val)]))))

(define var-p-matcht
  (lambda (var mval penv penv-out)
    (lambda (match?)
      (fresh (val)
        (symbolo var)
        (=/= 'closure mval)
        (condo [;; `var` is bound in `penv`
                (lookupt var penv val)
                (condo [;; mval matches
                        (==t mval val)
                        (== match? #t)
                        (== penv penv-out)]
                       [;; mval doesn't match -> fail
                        else
                        (=/= mval val)
                        (== penv penv-out)])]
               [;; `var` is not bound
                else
                (== match? #t)
                (== penv-out `((,var . (val . ,mval)) . ,penv))
                ((lookupt var penv 'not-bound) #f)])))))

(define p-matcht
  (lambda (p mval penv penv-out)
    (lambda (match?)
      (conde [(self-eval-literalo p)
              (== penv penv-out)
              ((==t p mval) match?)]
             [((var-p-matcht p mval penv penv-out) match?)]
             [(fresh (var pred val type)
                (== `(? ,pred ,var) p)
                (symbolo var)
                (conde [(== 'symbol? pred) (== type 'symbol)]
                       [(== 'number? pred) (== type 'number)])
                ((fresht ()
                   (typet mval type)
                   (var-p-matcht var mval penv penv-out))
                 match?))]
             [(fresh (quasi-p)
                (== (list 'quasiquote quasi-p) p)
                ((quasi-p-matcht quasi-p mval penv penv-out) match?))]))))

(define quasi-p-matcht
  (lambda (quasi-p mval penv penv-out)
    (lambda (match?)
      (conde [(literalo quasi-p)
              (== penv penv-out)
              ((==t quasi-p mval) match?)]
             [(fresh (p)
                (== (list 'unquote p) quasi-p)
                (condo [(p-matcht p mval penv penv-out)
                        (== match? #t)]
                       [else
                        (== match? #f) (=/= 'closure mval)]))]
             [(fresh (a d)
                (== `(,a . ,d) quasi-p)
                (=/= 'unquote a)
                (conde [(== match? #f)
                        (== penv penv-out)
                        (literalo mval)]
                       [(fresh (v1 v2 penv^)
                          (== `(,v1 . ,v2) mval)
                          ((fresht ()
                             (quasi-p-matcht a v1 penv  penv^)
                             (quasi-p-matcht d v2 penv^ penv-out))
                           match?))]))]))))
