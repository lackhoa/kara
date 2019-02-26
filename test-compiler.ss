(display
 (let ([x (var 'x)])
   ((== #t x) empty-c)))
(newline)

(display
 (let ([x (var 'x)]
       [y (var 'y)])
   ((conj2 (== #t x) (== y x))
    empty-c)))
(newline)

(display
 (let ([x (var 'x)]
       [y (var 'y)]
       [z (var 'z)])
   ((conj (=/= z x) (== #t x) (== x y))
    empty-c)))
(newline)

(display
 (let ([x (var 'x)]
       [y (var 'y)]
       [z (var 'z)])
   ((conj (=/= z x) (== #t x)(== y x) (== z #t))
    empty-c)))
(newline)

(display
 (let ([x (var 'x)])
   ((disj2 (== x 5) (== x 6)) empty-c)))
(newline)

(display
 (let ([x (var 'x)]
       [y (var 'y)])
   ((disj (== x 5) (== x 6) (=/= x y)) empty-c)))
(newline)

(printf "conde and fresh: x = 6 and y =/= 6\n")
(display
 ((fresh (x y)
    (conde
     [(== x 5) (== x y)]
     [(== x 6)])
    (=/= x y))
  empty-c))
(newline)

(printf "Shadowing\n")
(display
 ((fresh (x)
    (== x 5)
    (fresh (x)
      (== x 6)))
  empty-c))
(newline)

(printf "Run & Subsumption & Unification: x is not 5\n")
(display
 (run* (x y)
   (=/= x 5)
   (=/= `(,x ,y) '(5 6))
   (fresh (x)
     (=/= x 6))))
(newline)

(printf "==t\n")
(define ==t
  (lambda (x y)
    (lambda (t)
      (conde
       [(== #t t) (== x y)]
       [(== #f t) (=/= x y)]))))
(display
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
(display
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
(display
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
(display
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
(display
 (run* (x env t) ((lookupt x env t) #t)))
(newline)

(printf "lookupt -> not-in-envo\n")
(display
 (run* (x env t) ((lookupt x env t) #f)))
(newline)

(printf "Anti-unification: 2 * 2 = 2 + 2 vs 2 * 3 = 3 + 3\n")
(let-values ([(au S) (anti-unify '(2 * 2 = 2 + 2) '(2 * 3 = 3 + 3))])
  (display au) (newline) (display S))
(newline)

(printf "Anti-unification: Very big terms\n")
(let ([t1 '(#(0) ((#(0) val . #(1)) . #(2)) #(1))]
      [t2 '(#(0) ((#(0) rec . #(1)) . #(2)) (closure #(1) ((#(0) rec . #(1)) . #(2))))]
      [t3 '(#(0) ((#(1) . #(2)) . #(3)) #(4))])
  (let-values ([(au S) (anti-unify t1 t2 t3)])
    (display au) (newline) (display S)))
(newline)

(printf "minimize\n")
(display (apply minimize-uni (run* (x env t) ((lookupt x env t) #t))))
(newline)
