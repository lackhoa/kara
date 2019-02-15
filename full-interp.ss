;; The definition of 'letrec' is based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.

(define evalo
  (lambda (expr val)
    (eval-expo expr initial-env val)))

(define eval-expo
  ;; Keywords: quote, lambda, letrec, match, and, or, if,
  (lambda (expr env val)
    (condo
     [;; Self-eval: boolean, number
      (self-eval-literalt expr)
      (== expr val)]
     ;; [;; Variable lookup
     ;;  (typet expr 'symbol)
     ;;  ((lookupt expr env val) #t)]
     ;; [;; Pairs
     ;;  else
     ;;  (fresh (tag expr-d function)
     ;;    (== expr `(,tag . ,expr-d))
     ;;    (condo
     ;;     [(typet tag 'symbol)
     ;;      (condo
     ;;       [(lookupt tag env function) succeed]
     ;;       [;; Special forms and macros
     ;;        else
     ;;        (;; Signal to the code below
     ;;         == function #f)
     ;;        (condo
     ;;         [;; Quotation: cannot create funval
     ;;          (==t tag 'quote)
     ;;          (== expr-d `(,val))
     ;;          ((typet val 'pair) #t)]
     ;;         [;; Lambda
     ;;          (==t tag 'lambda)
     ;;          (fresh (x body)
     ;;            (== `(,x ,body) expr-d)
     ;;            (== `(closure (lambda ,x ,body) ,env) val)
     ;;            (conde
     ;;             [;; Variadic
     ;;              (symbolo x)]
     ;;             [;; Multi-argument
     ;;              (list-of-symbolso x)]))]
     ;;         [;; Pattern matching
     ;;          (==t tag 'match)
     ;;          (handle-matcho expr env val)]
     ;;         [;; Single-function variadic letrec version
     ;;          (==t tag 'letrec)
     ;;          (fresh (p-name x body letrec-body)
     ;;            (== `(letrec [(,p-name (lambda ,x ,body))]
     ;;                  ,letrec-body)
     ;;               expr)
     ;;            (conde
     ;;             [;; Variadic
     ;;              (symbolo x)]
     ;;             [;; Multi-argument
     ;;              (list-of-symbolso x)])
     ;;            (eval-expo letrec-body
     ;;                       `((,p-name . (rec . (lambda ,x ,body))) . ,env)
     ;;                       val))]
     ;;         [;; Macros
     ;;          else (prim-expo expr env val)])])]
     ;;     [(typet tag 'pair)
     ;;      ;; Definitely application
     ;;      (eval-expo tag env function)])
     ;;    (;; Do function application if needed
     ;;     ifo (==t #f function) succeed
     ;;         (let ([rands expr-d])
     ;;           (fresh (fun-tag fun-d a* x)
     ;;             (== function `(,fun-tag . ,fun-d))
     ;;             (eval-listo rands env a*)
     ;;             (condo
     ;;              [(==t fun-tag 'closure)
     ;;               (fresh (body env^ env-res)
     ;;                 (== `((lambda ,x ,body) ,env^) fun-d)
     ;;                 (condo
     ;;                  [;; Variadic
     ;;                   (typet x 'symbol)
     ;;                   (== `((,x . (val . ,a*)) . ,env^) env-res)
     ;;                   (eval-expo body env-res val)]
     ;;                  [;; Multi-argument
     ;;                   else
     ;;                   (let ([x* x])
     ;;                     (ext-env*o x* a* env^ env-res))
     ;;                   (eval-expo body env-res val)]))]
     ;;              [;; Primitive
     ;;               (==t fun-tag 'prim)
     ;;               (let ([prim-id fun-d])
     ;;                 (eval-primo prim-id a* val))])))))]
     )))

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

(define list-of-symbolso
  (lambda (los)
    (conde
     [(== '() los)]
     [(symbolo los)]
     [(fresh (a d)
        (== `(,a . ,d) los)
        (symbolo a)
        (list-of-symbolso d))])))

(define eval-listo
  ;; To handle variadic case
  (lambda (expr env val)
    (conde
     [(== '() expr)
      (== '() val)]
     [(fresh (a d v-a v-d)
        (== `(,a . ,d) expr)
        (== `(,v-a . ,v-d) val)
        (eval-expo a env v-a)
        (eval-listo d env v-d))])))

(define ext-env*o
  (lambda (x* a* env env-out)
    (conde
     [(== '() x*) (== '() a*) (== env env-out)]
     [;; Handles rest-arg case
      (symbolo x*)
      (== `((,x* . (val . ,a*)) . ,env) env-out)]
     [(fresh (x a dx* da* env2)
        (== `(,x . ,dx*) x*)
        (== `(,a . ,da*) a*)
        (== `((,x . (val . ,a)) . ,env) env2)
        (symbolo x)
        (ext-env*o dx* da* env2 env-out))])))

(define eval-primo
  (lambda (prim-id a* val)
    (condo
     [(==t prim-id 'cons)
      (fresh (a d)
        (== `(,a ,d) a*)
        (== `(,a . ,d) val))
      (;; Cannot create funvar
       (typet val 'pair) #t)]
     [(==t prim-id 'car)
      (fresh (d)
        (== `((,val . ,d)) a*))]
     [(==t prim-id 'cdr)
      (fresh (a)
        (== `((,a . ,val)) a*))]
     [(==t prim-id 'not)
      (fresh (b)
        (== `(,b) a*)
        ((==t #f b) val))]
     [(==t prim-id 'equal?)
      (fresh (v1 v2)
        (== `(,v1 ,v2) a*)
        ((==t v1 v2) val))]
     [(==t prim-id 'symbol?)
      (fresh (v sort)
        (== `(,v) a*)
        ((typet v 'symbol) val))]
     [(==t prim-id 'null?)
      (fresh (v)
        (== `(,v) a*)
        ((==t '() v) val))])))

(define prim-expo
  (lambda (expr env val)
    (conde
     [(boolean-primo expr env val)]
     [(and-primo     expr env val)]
     [(or-primo      expr env val)]
     [(if-primo      expr env val)])))

(define boolean-primo
  (lambda (expr env val)
    (conde
     [(== #t expr) (== #t val)]
     [(== #f expr) (== #f val)])))

(define and-primo
  (lambda (expr env val)
    (fresh (e*)
      (== `(and . ,e*) expr)
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
      (eval-expo e1 env t)
      (if (==t #f t) (eval-expo e3 env val)
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

;;; Pattern matching business

(define handle-matcho
  (lambda (expr env val)
    (fresh (against-expr mval clause clauses)
      (== `(match ,against-expr ,clause . ,clauses) expr)
      (eval-expo against-expr env mval)
      (match-clauses mval `(,clause . ,clauses) env val))))

(define typet
  ;; Complete classification of recognized terms
  ;; also able to determine if a term is of some type
  (lambda (term type)
    (lambda (same?)
      (fresh (T)
        ((==t type T) same?)
        (condo
         [(==t 'symbol  T) (symbolo term)]
         [(==t 'boolean T) (conde [(== term #t)]
                                  [(== term #f)])]
         [(==t 'number  T) (numbero term)]
         [(==t 'null    T) (== '() term)]
         [else
          (fresh (a _d)
            (== `(,a . ,_d) term)
            (condo
             [(disjt (==t a 'prim) (==t a 'closure))
              (== 'funval T)]
             [else (== 'pair T)]))])))))

(define self-eval-literalt
  (lambda (term)
    (disjt (typet term 'boolean)
           (typet term 'number))))

(define regular-env-appendo
  (lambda (env1 env2 env-out)
    (conde
     [(== empty-env env1) (== env2 env-out)]
     [(fresh (y v rest res)
        (== `((,y . (val . ,v)) . ,rest) env1)
        (== `((,y . (val . ,v)) . ,res) env-out)
        (regular-env-appendo rest env2 res))])))

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
          ((lookupt var penv 'unbound) #f)
          (== penv-out `((,var . (val . ,mval)) . ,penv))])))))

(define p-matcht
  (lambda (p mval penv penv-out)
    (lambda (match?)
      (condo
       [(self-eval-literalt p)
        ((==t p mval) match?)
        (== penv penv-out)]

       [(typet p 'symbol)
        ((var-p-matcht p mval penv penv-out)
         match?)]

       [(fresh (ignore)
          (==t `(? . ,ignore)))
        (fresh (var pred val type)
          (== `(? ,pred ,var) p)
          (symbolo var)
          (conde
           [(== 'symbol? pred) (== type 'symbol)]
           [(== 'number? pred) (== type 'number)])
          ((conjt
            (typet mval type)
            (var-p-matcht var mval penv penv-out))
           match?))]

       [else
        (fresh (quasi-p)
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
            ((p-matcht d mval penv penv-out) match?)]
           [else
            (condo
             [(typet mval 'pair)
              (fresh (v1 v2 penv^)
                (== `(,v1 . ,v2) mval)
                ((conjt
                  (quasi-p-matcht a v1 penv  penv^)
                  (quasi-p-matcht d v2 penv^ penv-out))
                 match?))]
             [else
              (== match? #f)
              (== penv penv-out)])]))]
       [else
        ((==t quasi-p mval) match?)
        (== penv penv-out)]))))
