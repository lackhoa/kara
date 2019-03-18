;;; Functions
(define memberd
  (lambda (x ees)
    (fresh (e es)
      (== ees `(,e . ,es))
      (condo
       [(==t x e) succeed]
       [else (fake `(memberd ,x ,es))]))))
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
            (fake `((lookupt ,x ,rest ,t) ,bound?))]))]))))
(define typet
  ;; Complete classification of recognized terms
  ;; also able to determine if a term is of some type
  (lambda (term type)
    (lambda (same?)
      (fresh (T)
        ((==t type T) same?)
        (conde
         [(== 'symbol  T) (fake `(symbolo ,term))]
         [(== 'boolean T) (conde [(== term #t)]
                                [(== term #f)])]
         [(== 'number  T) (fake `(numbero ,term))]
         [(== 'null    T) (== '() term)]
         [(fresh (a _d)
            (== `(,a . ,_d) term)
            (condo
             [(disjt (==t a 'prim) (==t a 'closure))
              (== 'funval T)]
             [else (== 'pair T)]))])))))
(define vart (lambda (t) (lambda (?) (fake `(vector?o ,t ,?)))))
(define pairt (lambda (t) (lambda (?) (fake `(pair?o ,t ,?)))))
(define fake-unify
  (lambda (t1 t2 S S+)
    (fresh (t1+ t2+)
      (fake `(walko ,t1 ,S ,t1+))
      (fake `(walko ,t2 ,S ,t2+))
      (condo
       [(==t t1+ t2+) (== S S+)]
       [(vart t1+) (fake `(ext-S-checko ,t1+ ,t2+ ,S ,S+))]
       [(vart t2+) (fake `(ext-S-checko ,t2+ ,t1+ ,S ,S+))]
       [(conjt (pairt t1+) (pairt t2+))
        (fresh (a1 a2 d1 d2 S^)
          (== `(,a1 . ,d1) t1+)
          (== `(,a2 . ,d2) t2+)
          (fake `(unifyo ,a1 ,a2 ,S ,S^))
          (condo [(=/=t S^ #f)
                  (fake `(unifyo ,d1 ,d2 ,S^ ,S+))]))]
       [(==t t1+ t2+) (== S+ S)]
       [else (== S+ #f)]))))

;;; Tests
(pp "=> nothing\n")
(pp
 ((fresh (x y z)
    (conj (=/= z x) (== #t x)(== y x) (== z #t)))
  init-c))
(newline)

(pp "conde and fresh => x = 6 and y =/= 6\n")
(pp
 ((fresh (x y)
    (conde
     [(== x 5) (== x y)]
     [(== x 6)])
    (=/= x y))
  init-c))
(newline)

(pp "Shadowing => x is both 5 and 6\n")
(pp
 ((fresh (x)
    (== x 5)
    (fresh (x)
      (== x 6)))
  init-c))
(newline)

(pp "Run & Subsumption & Reification involved")
(pp "=> x is not 5\n")
(pp
 (run* (x y)
   (=/= x 5)
   (=/= `(,x ,y) '(5 6))
   (fresh (x)
     (=/= x 6))))
(newline)

(pp "=> a bit more than nothing\n")
(pp (run* (x)
      (fresh (x) (=/= x 5))))
(newline)

(pp "==t => x is different from y\n")
(pp
 (run* (x y) ((==t x y) #f)))
(newline)

(pp "conjt => x is y and y is z\n")
(pp
 (run* (x y z t)
   ((conjt (==t x y) (==t y z)) t)))
(newline)

(pp "disjt => x is y or y is z\n")
(pp
 (run* (x y z t) ((disjt (==t x y) (==t y z)) t)))
(newline)

(pp "condo & memberd => (memberd x x*)\n")
(pp
 (run* (x x*) (memberd x x*)))
(newline)

(pp "typet => not pair\n")
(pp (run* (term) ((typet term 'pair) #f)))
(newline)

(pp "lookupt => lookupo\n")
(pp
 (run* (x env t) ((lookupt x env t) #t)))
(newline)

(pp "lookupt => not-in-envo\n")
(pp
 (run* (x env t) ((lookupt x env t) #f)))
(newline)

(pp "Anti-unification => 2*x=x+x")
(let-values ([(au S) (anti-unify '(2 * 2 = 2 + 2) '(2 * 3 = 3 + 3))])
  (pp au) (pp S))
(newline)

(pp "Anti-unification: => (x (? z))\n")
(let ([x (var 'x 0)] [z (var 'z 0)] [y (var 'y 0)])
  (let ([t1 `(,x (,x ,z))]
        [t2 `(,x (,y ,z))]
        [t3 `(,x (,x ,z))])
    (let-values ([(au S) (anti-unify t1 t2 t3)])
      (pp au) (pp S))))
(newline)

(pp "run*au test: x is (z u z): u could be y or not\n")
(pp
 (run*au (x y z)
         (fresh (u v)
           (== `(,z ,u ,v) x)
           (conde
            [(== u y) (== z v)]
            [(=/= u y) (== z v)]))))
(newline)

(pp "run*au on lookupo\n")
(pp (run*au (x env t) ((lookupt x env t) #t)))
(newline)

(pp "How does run*au deal with permutation? => x == y")
(pp (run*au (x y) (conde [(== y x)] [(== x y)])))

(pp "run*au on memberd")
(pp (run*au (x ees) (memberd x ees)))
(newline)

(pp "run* on memberd")
(pp (run* (x ees) (memberd x ees)))
(newline)


#!eof

(pp "Time for it to work on itself!")
(pp (run* (t1 t2 S S+) (fake-unify t1 t2 S S+)))
