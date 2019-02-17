;; The definition of 'letrec' is based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.
;; This thing is full of 'match' and 'letrec' (not cool!)

(define evalo
  (lambda (expr val)
    (eval-expo expr initial-env val)))

(define eval-expo
  ;; Keywords: quote, lambda, letrec, match, and, or, if
  (lambda (expr env val)
    (conde
     [(self-eval-literalo expr)
      (== expr val)]
     [(symbolo expr)
      (lookupo expr env val)]
     [;; Quotation: cannot introduce funval
      (== expr `(quote ,val))
      (not-in-envo 'quote env)
      (absento 'closure val) (absento 'prim val)]
     [(handle-lambdao expr env val)]
     [(handle-matcho expr env val)]
     [(handle-letreco expr env val)]
     [(prim-expo expr env val)]
     [(applyo expr env val)])))

(define applyo
  (lambda (expr env val)
    (fresh (rator rands function a*)
      (== `(,rator . ,rands) expr)
      (eval-expo rator env function)
      (let ([make-arg (eval-exp*o rands env a*)])
        (conde
         [;; Compound
          (fresh (para body env^ env-res)
            (== `(closure (lambda ,para ,body) ,env^)
               function)
            make-arg
            (ext-env*o para a* env^ env-res)
            (eval-expo body env-res val))]
         [;; Primitive
          (fresh (prim-id)
            (== `(prim . ,prim-id) function)
            make-arg
            (apply-primo prim-id a* val))])))))

(define handle-lambdao
  (lambda (expr env val)
    (fresh (para body)
      (== `(lambda ,para ,body) expr)
      (not-in-envo 'lambda env)
      (== `(closure (lambda ,para ,body) ,env) val)
      (para-listo para))))

(define handle-letreco
  ;; Single-function variadic letrec version
  (lambda (expr env val)
    (fresh (p-name x body letrec-body)
      (== `(letrec [(,p-name (lambda ,x ,body))]
            ,letrec-body)
         expr)
      (not-in-envo 'letrec env)
      (para-listo x)
      (eval-expo letrec-body
                 `((,p-name . (rec . (lambda ,x ,body))) . ,env)
                 val))))

(define prim-expo
  (lambda (expr env val)
    (conde
     [(and-primo expr env val)]
     [(or-primo expr env val)]
     [(if-primo expr env val)])))

(define empty-env '())

(define lookupt
  (lambda (x env t)
    (lambda (bound?)
      (conde
       [(== empty-env env)
        (== bound? #f) (== t 'unbound)]
       [(fresh (y b rest)
          (== `((,y . ,b) . ,rest) env)
          (condo
           [(==t x y)
            (== bound? #t)
            (conde
             [(== `(val . ,t) b)]
             [(fresh (lam-expr)
                (== `(rec . ,lam-expr) b)
                (== `(closure ,lam-expr ,env) t))])]
           [else
            ((lookupt x rest t) bound?)]))]))))

(define lookupo
  (lambda (x env val)
    ((lookupt x env val) #t)))

(define not-in-envo
  (lambda (x env)
    ((lookupt x env 'unbound) #f)))

(define para-listo
  (lambda (los)
    (conde
     [(== '() los)]
     [;; Rest arg
      (symbolo los)]
     [(fresh (a d)
        (== `(,a . ,d) los)
        (symbolo a)
        (para-listo d))])))

(define eval-exp*o
  ;; to make arg list
  (lambda (expr env val)
    (conde
     [(== '() expr) (== '() val)]
     [(fresh (a d v-a v-d)
        (== `(,a . ,d) expr)
        (== `(,v-a . ,v-d) val)
        (eval-expo a env v-a)
        (eval-exp*o d env v-d))])))

(define ext-env*o
  (lambda (x* a* env env-out)
    (conde
     [(== '() x*) (== '() a*) (== env env-out)]
     [;; Rest arg
      (symbolo x*)
      (== `((,x* . (val . ,a*)) . ,env) env-out)]
     [(fresh (x a dx* da* env2)
        (== `(,x . ,dx*) x*)
        (== `(,a . ,da*) a*)
        (== `((,x . (val . ,a)) . ,env) env2)
        (symbolo x)
        (ext-env*o dx* da* env2 env-out))])))

(define apply-primo
  (lambda (prim-id a* val)
    (conde
     [;; cons cannot create funvar, since we cannot quote
      ;; anything containing prim and closure (an overkill!)
      (== prim-id 'cons)
      (fresh (a d)
        (== `(,a ,d) a*)
        (== `(,a . ,d) val))]
     [(== prim-id 'car)
      (fresh (d)
        (== `((,val . ,d)) a*)
        ((typet `(,val . ,d) 'pair) #t))]
     [(== prim-id 'cdr)
      (fresh (a)
        (== `((,a . ,val)) a*)
        ((typet `(,a . ,val) 'pair) #t))]
     [(== prim-id 'not)
      (fresh (b)
        (== `(,b) a*)
        ((==t #f b) val))]
     [(== prim-id 'equal?)
      (fresh (v1 v2)
        (== `(,v1 ,v2) a*)
        ((==t v1 v2) val))]
     [(== prim-id 'symbol?)
      (fresh (v sort)
        (== `(,v) a*)
        ((typet v 'symbol) val))]
     [(== prim-id 'null?)
      (fresh (v)
        (== `(,v) a*)
        ((==t '() v) val))])))

(define and-primo
  (lambda (expr env val)
    (fresh (e*)
      (== `(and . ,e*) expr)
      (not-in-envo 'and env)
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
        (condo
         [(==t #f v)
          (== #f val)
          (eval-expo e1 env v)]
         [else
          (eval-expo e1 env v)
          (ando `(,e2 . ,e-rest) env val)]))])))

(define or-primo
  (lambda (expr env val)
    (fresh (e*)
      (== `(or . ,e*) expr)
      (not-in-envo 'or env)
      (oro e* env val))))

(define oro
  (lambda (e* env val)
    (conde
     [(== '() e*) (== #f val)]
     [(fresh (e)
        (== `(,e) e*)
        (eval-expo e env val))]
     [(fresh (e1 e2 e-rest v)
        (== `(,e1 ,e2 . ,e-rest) e*)
        (condo
         [(==t #f v)
          (eval-expo e1 env v)
          (oro `(,e2 . ,e-rest) env val)]
         [else
          (== v val)
          (eval-expo e1 env v)]))])))

(define if-primo
  (lambda (expr env val)
    (fresh (e1 e2 e3 t)
      (== `(if ,e1 ,e2 ,e3) expr)
      (not-in-envo 'if env)
      (eval-expo e1 env t)
      (ifo (==t #f t) (eval-expo e3 env val)
           (eval-expo e2 env val)))))

(define initial-env
  `((list    . (val . (closure (lambda x x) ,empty-env)))
    (not       . (val . (prim . not)))
    (equal?  . (val . (prim . equal?)))
    (symbol? . (val . (prim . symbol?)))
    (cons    . (val . (prim . cons)))
    (null?   . (val . (prim . null?)))
    (car     . (val . (prim . car)))
    (cdr     . (val . (prim . cdr)))
    . ,empty-env))

(define typet
  ;; Complete classification of recognized terms
  ;; also able to determine if a term is of some type
  (lambda (term type)
    (lambda (same?)
      (fresh (T)
        ((==t type T) same?)
        (conde
         [(== 'symbol  T) (symbolo term)]
         [(== 'boolean T) (conde [(== term #t)]
                                [(== term #f)])]
         [(== 'number  T) (numbero term)]
         [(== 'null    T) (== '() term)]
         [(fresh (a _d)
            (== `(,a . ,_d) term)
            (condo
             [(disjt (==t a 'prim) (==t a 'closure))
              (== 'funval T)]
             [else (== 'pair T)]))])))))

(define self-eval-literalo
  (lambda (term)
    (conde [(== #t term)] [(== #f term)]
           [(numbero term)])))

(define regular-env-appendo
  (lambda (env1 env2 env-out)
    (conde
     [(== empty-env env1) (== env2 env-out)]
     [(fresh (y v rest res)
        (== `((,y . (val . ,v)) . ,rest) env1)
        (== `((,y . (val . ,v)) . ,res) env-out)
        (regular-env-appendo rest env2 res))])))

;;; Pattern matching business

(define handle-matcho
  (lambda (expr env val)
    (fresh (against-expr mval clause clauses)
      (== `(match ,against-expr ,clause . ,clauses) expr)
      (not-in-envo 'match env)
      (eval-expo against-expr env mval)
      (match-clauses mval `(,clause . ,clauses) env val))))

(define match-clauses
  (lambda (mval clauses env val)
    (fresh (p result-expr d penv)
      (== `((,p ,result-expr) . ,d) clauses)
      (condo
       [(p-matcht p mval '() penv)
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
        (condo
         [;; `var` is bound in `penv`
          (lookupt var penv val)
          ((==t mval val) match?)
          (ifo (==t match? #t) (=/= 'closure mval)
               succeed)
          (== penv penv-out)]
         [;; `var` is not bound -> auto match
          (==t match? #t)
          (not-in-envo var penv)
          (== penv-out `((,var . (val . ,mval)) . ,penv))])))))

(define p-matcht
  (lambda (p mval penv penv-out)
    (lambda (match?)
      (conde
       [(self-eval-literalo p)
        ((==t p mval) match?)
        (== penv penv-out)]

       [((var-p-matcht p mval penv penv-out)
         match?)]

       [(fresh (var pred val type)
          (== `(? ,pred ,var) p)
          (symbolo var)
          (conde
           [(== 'symbol? pred) (== 'symbol type)]
           [(== 'number? pred) (== 'number type)])
          ((conjt
            (typet mval type)
            (var-p-matcht var mval penv penv-out))
           match?))]

       [(fresh (quasi-p)
          (== (list 'quasiquote quasi-p) p)
          ((quasi-p-matcht quasi-p mval penv penv-out) match?))]))))

(define quasi-p-matcht
  (lambda (quasi-p mval penv penv-out)
    (lambda (match?)
      (condo
       [(typet quasi-p 'pair)
        (fresh (a d)
          (== `(,a . ,d) quasi-p)
          (condo
           [(==t 'unquote a)
            (fresh (p)
              (== `(,p) d)
              ((p-matcht p mval penv penv-out) match?))]
           [(typet mval 'pair)
            (fresh (v1 v2 penv^)
              (== `(,v1 . ,v2) mval)
              ((conjt
                (quasi-p-matcht a v1 penv  penv^)
                (quasi-p-matcht d v2 penv^ penv-out))
               match?))]
           [(==t match? #f)
            (== penv penv-out)]))]
       [(typet quasi-p 'funval) fail]
       [else
        ((==t quasi-p mval) match?)
        (== penv penv-out)]))))
