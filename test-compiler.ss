(printf "This will return nothing\n")
(pp
 (fresh (x y z)
   ((conj (=/= z x) (== #t x)(== y x) (== z #t))
    empty-c)))
(newline)

(printf "conde and fresh: x = 6 and y =/= 6\n")
(pp
 ((fresh (x y)
    (conde
     [(== x 5) (== x y)]
     [(== x 6)])
    (=/= x y))
  empty-c))
(newline)

(printf "Shadowing: x is both 5 and 6\n")
(pp
 ((fresh (x)
    (== x 5)
    (fresh (x)
      (== x 6)))
  empty-c))
(newline)

(printf "Run & Subsumption & Reification: x is not 5\n")
(pp
 (run* (x y)
   (=/= x 5)
   (=/= `(,x ,y) '(5 6))
   (fresh (x)
     (=/= x 6))))
(newline)

(printf "==t: x is different from y\n")
(define ==t
  (lambda (x y)
    (lambda (t)
      (conde
       [(== #t t) (== x y)]
       [(== #f t) (=/= x y)]))))
(pp
 (run* (x y) ((==t x y) #f)))
(newline)

(printf "conjt\n")
(define-syntax conjt
  ;; Conjunction of test
  (syntax-rules ()
    [(_ g) g]
    [(_ g1 g2 gs ...)
     (lambda (t)
       (conde
        [(g1 #t)  ((conjt g2 gs ...) t)]
        [(== #f t) (g1 #f)]))]))
(pp
 (run* (x y z t)
   ((conjt (==t x y) (==t y z)) t)))
(newline)

(printf "disjt\n")
(define-syntax disjt
  ;; A disjunction test
  (syntax-rules ()
    [(_ g) g]
    [(_ g1 g2 gs ...)
     (lambda (t)
       (conde
        [(== #t t) (g1 #t)]
        [(g1 #f)  ((disjt g2 gs ...) t)]))]))
(pp
 (run* (x y z t) ((disjt (==t x y) (==t y z)) t)))
(newline)

(printf "condo & memberd\n")
(define-syntax condo
  ;; Literally the relational version of 'cond'
  ;; Fails if no clauses match
  (syntax-rules (else)
    [(_ [else g]) g]
    [(_ [else g1 g2 g* ...])
     (fresh () g1 g2 g* ...)]
    [(_ [test g g* ...] c* ...)
     (conde
      [(test #t) g g* ...]
      [(test #f) (condo c* ...)])]
    [(_) fail]))
(define memberd
  (lambda (x ees)
    (fresh (e es)
      (== ees `(,e . ,es))
      (condo
       [(==t x e) succeed]
       [else (fake-goal `(memberd ,x ,es))]))))
(pp
 (run* (x x*) (memberd x x*)))
(newline)

(printf "lookupt -> lookupo\n")
(define lookupt
  (lambda (x env t)
    (lambda (bound?)
      (conde
       [(== '() env)
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
            (fake-goal `((lookupt ,x ,rest ,t) ,bound?))]))]))))
(pp
 (run* (x env t) ((lookupt x env t) #t)))
(newline)

(printf "lookupt -> not-in-envo\n")
(pp
 (run* (x env t) ((lookupt x env t) #f)))
(newline)

;; (printf "Anti-unification: 2 * 2 = 2 + 2 vs 2 * 3 = 3 + 3\n")
;; (let-values ([(au S) (anti-unify '(2 * 2 = 2 + 2) '(2 * 3 = 3 + 3))])
;;   (pp au) (newline) (pp S))
;; (newline)

;; (printf "Anti-unification: Big terms\n")
;; (let ([t1 '(#(x) ((#(x) val . #(t)) . #(2)) #(t))]
;;       [t2 '(#(x) ((#(x) rec . #(1)) . #(2)) (closure #(1) ((#(x) rec . #(1)) . #(2))))]
;;       [t3 '(#(x) ((#(1) . #(2)) . #(3)) #(t))])
;;   (let-values ([(au S) (anti-unify t1 t2 t3)])
;;     (pp au) (newline) (pp S)))
;; (newline)

(printf "typet\n")
(define typet
  ;; Complete classification of recognized terms
  ;; also able to determine if a term is of some type
  (lambda (term type)
    (lambda (same?)
      (fresh (T)
        ((==t type T) same?)
        (conde
         [(== 'symbol  T) (fake-goal `(symbolo ,term))]
         [(== 'boolean T) (conde [(== term #t)]
                                 [(== term #f)])]
         [(== 'number  T) (fake-goal `(numbero ,term))]
         [(== 'null    T) (== '() term)]
         [(fresh (a _d)
            (== `(,a . ,_d) term)
            (condo
             [(disjt (==t a 'prim) (==t a 'closure))
              (== 'funval T)]
             [else (== 'pair T)]))])))))
(pp
 (run* (term)
   ((typet term 'pair) #f)))
(newline)

(printf "run*min test: x is (z u): u could be y or not\n")
(pp
 (run*min (x y z)
          (fresh (u)
            (== `(,z ,u) x)
            (conde
             [(== u y)]
             [(=/= u y)]))))
(newline)

(printf "run*min on lookupo\n")
(pp
 (run*min (x env t)
          ((lookupt x env t) #t)))
(newline)



#!eof
