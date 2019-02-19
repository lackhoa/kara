(define lookupts
  (lambda (x env t)
    (lambda (bound?)
      (condes
       [(==s #f bound?) (==s empty-env env)]
       [(freshs (y b rest)
                (==s `((,y . ,b) . ,rest) env)
                (condes
                 [(==s x y) (==s #t bound?)
                  (condes
                   [(== `(val . ,t) b)]
                   [(freshs (lam-expr)
                            (==s `(rec . ,lam-expr) b)
                            (==s `(closure ,lam-expr ,env) t))])]
                 [(=/=s x y)
                  ((lookupt x rest t) bound?)]))]))))

;; A static expression is a list of conde clauses (disjunctive normal form)
;; first: ([a] [b])
;; second: ([c] [d])
;; first & second: ([([a] [b]) ([c] [d])])
;; first or second: ([a] [b] [c] [d])

(define succeeds  '([]))
(define succeeds? (lambda (e) (equal? e succeeds)))
(define fails     '())
(define fails?    (lambda (e) (equal? e fails)))

(define dummy-var?
  (lambda (e)
    (cond
     [(pair? e) (eq? (car e) 'dummy-var)]
     [else #f])))

(define ==s
  (lambda (u v)
    (cond
     [(or (dummy-var? u) (dummy-var? v)) `([(== ,u ,v)])]
     [(equal? u v) succeeds]
     [else fails])))

(define conjs
  (lambda (e0 e1)
    (cond
     [(or (fails? e0) (fails? e1)) fails]
     [(succeeds? e0) e1]
     [(succeeds? e1) e0]
     [else `([,e0 ,e1])])))

(define-syntax conj*s
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e1 e* ...)
     (conj*s (conjs e0 e1) e* ...)]))

(define disjs
  (lambda (e0 e1)
    (cond
     [(or (succeeds? e0) (succeeds? e1)) succeeds]
     [(fails? e0) e1]
     [(fails? e1) e0]
     [else `(,@e0 ,@e1)])))

(define-syntax disjs
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e1 e* ...)
     (condes (disjs e0 e1) e* ...)]))
