#|Notes
Expressions:
* Compound formula:
** Binder
** Binary Connectives
* Atomic formula: [Sym . [Term]]
* Compound term: [Sym . [Term]]
* Var: Sym

Suspicious patterns:
* Every tactics work on the goal
|#

;;; Macros & helpers
(define for
  (lambda (f)
    (let loop ([vs (free-vars f '())])
      (cond
       [(null? vs) f]
       [else `(forall ,(car vs) ,(loop (cdr vs)))]))))

(define free-vars
  (lambda (f bound)
    (dedup
     (pmatch f
       "free-vars"
       [(,bi ,v ,f)
        (guard (binder? bi))
        (free-vars f `(,v ,@bound))]
       [(,bc ,f ,g)
        (guard (bc? binary-connectors))
        `(,@(free-vars f bound)
          ,@(free-vars g bound))]
       [(? . ,args)
        (apply append
          (map (lambda (t) (free-vars-term t bound)) args))]))))

(define free-vars-term
  (lambda (t bound)
    (let free-vars-term ([t t])
      (pmatch t
        "free-vars-term"
        [(,fun . ,args)
         (apply append
           (map free-vars-term args))]
        [,x
         (guard (var? x))
         (if (memq x bound) '() `(,x))]))))

(define sub
  (lambda (x s f)
    (pmatch f
      [(,bi ,y ,g)
       (guard (binder? bi))
       (cond
        [(eq? x y) f]
        [(memq y (free-vars-term s '()))
         (let ([y* (freshen y
                            `(,x
                              ,@(free-vars-term s '())
                              ,@(free-vars g '())))])
           (let ([g (sub y y* g)])
             (let ([g (sub x s g)])
               `(,bi ,y* ,g))))]
        [else
         `(,bi ,y ,(sub x s g))])]
      [(,bc ,f ,g)
       (guard (bc? bc))
       `(and ,(sub x s f) ,(sub x s g))]
      [(,pred . ,args)
       `(,pred ,@(map (lambda (t) (sub-term x s t)) args))]
      [else (error 'sub "Not a legal formula" f)])))

(define sub-term
  (lambda (x s t)
    (pmatch t
      [(,fun . ,args)
       (let ([sub (lambda (arg) (sub-term x s arg))])
         `(,fun ,@(map sub args)))]
      [,y
       (guard (var? x))
       (if (eq? x y) s y)]
      [else (error 'sub-term "Not a legal term" t)])))

(define inst
  (lambda (f x)
    (pmatch f
      [(,bi ,y ,g)
       (guard (binder? bi))
       (sub y x g)]
      [else (error 'inst "Not a binding formula" f)])))

;;; Definitions
(define binary-connectors '(-> and or <->))
(define bc? (lambda (s) (member s binary-connectors)))
(define binders '(forall exists))
(define binder? (lambda (s) (member s binders)))
(define var? symbol?)
(define make-env (lambda (v a g q) `(,v ,a ,g ,q)))
(define init-env (lambda (g) (make env '() '() g '())))
(define terminal-env (make env '() '() #t '()))
(define done
  (lambda (env)
    (let ([q (env-g env)])
      (pmatch q
        [() terminal-env]
        [((,v ,a ,g) . ,rest)
         (make env v a g rest)]))))

(define-syntax go
  (syntax-rules ()
    [(_) (lambda (x) x)]
    [(_ f f* ...) (lambda (x) ((go f* ...) (f x)))]))

(define-syntax prove
  (syntax-rules ()
    [(_ g steps ...)
     (print-env ((go steps ...)
                 (init-env g)))]))

(define print-env
  (lambda (env)
    (cond
     [(eq? terminal-env env) (pp "All done!")]
     [else
      (pmatch env
        [(,v ,a ,g ,q)
         (begin
           (display "Vars: ") (for-each (lambda (v) (printf "~a, " v)) v)
           (newline) (newline)
           (pp "Assets:") (for-each pp a)
           (newline)
           (pp (format "Goal (~a left):" (length q))
               g))])])))

(define add-var
  (lambda (x)
    (lambda (env)
      (pmatch env
        [(,v ,a ,g ,q)
         ((,x ,@v) ,a ,g ,q)]))))

;;; Tactics (inference rules)
(define intro
  (lambda (env)
    (pmatch g
      [(forall ,x ,f)
       (let ([x* (freshen-env x env)])
         (let ([g (sub x x* f)])
           ((go (add-var x*) (assert g) done)
            env)))]
      [(-> ,ante ,conse)
       (make env v `(,ante ,@a) conse q)]
      [else (error 'intro "Expected a forall or a ->" g)])))

(define freshen
  ;; Str -> [Var] -> Var
  (lambda (name used)
    (let freshen ([name (sy->str name)])
      (cond
       [(memq (str->sy name) used)
        (freshen (str-app name "*"))]
       [else (str->sy name)]))))

(define gain
  ;; Gain a new formula for free
  (lambda (f)
    (lambda (env)
      (pmatch env
        [(,v ,a ,g ,q)
         (make-env v `(,f ,@a) g q)]))))

(define argue
  ;; Prove something for no reason
  (lambda (f)
    (lambda (env)
      (pmatch env
        [(,v ,a ,g ,q)
         (make-env v a g `((,v ,a ,f) ,@q))]))))

(define assert
  ;; Prove it and thou shall get it
  (lambda (f) (go (argue f) (gain f))))

(define <->-intro
  (gtr
   [(<-> ,f ,g)
    `(and (-> ,f ,g) (-> ,g ,f))]))

(define ind
  (lambda (env)
    (pmatch env
      [(,v ,a ,g ,q)
       ((go
         (argue (inst g '(0)))
         (let ([n (freshen "n" (free-vars g))])
           (argue `(forall ,n
                      (-> ,(inst g n)
                         ,(inst g `(s ,n))))))
         done)
        env)])))

(define inj
  (gtr
   [(= (,fun . ,args1) (,fun . ,args2))
    (let loop ([args1 args1]
               [args2 args2])
      (pmatch `(,args1 ,args2)
        [(() ()) '()]
        [((,a1 . ,d1) (,a2 . ,d2))
         `(,(= ,a1 ,a2) ,@(loop d1 d2))]
        [else (error 'inj "Arity mismatch" g)]))]
   [else (error 'inj "Tactic does not apply" g)]))

(define rwl
  (lambda (l*)
    (gtr
     [(= ,l ,r) `((= ,l* ,r))]
     [else (error 'rwr "Not an equality" g)])))

(define rwr
  (lambda (r*)
    (gtr
     [(= ,l ,r) `((= ,l ,r*))]
     [else (error 'rwr "Not an equality" g)])))

;;; Axioms
;; Arithmetic
(define plus0  (for '(= (+ (0) n) m)))
(define plus-s (for '(= (+ (s n) m) (s (+ n m)))))
(define plus0r (for '(= (+ n (0)) n)))
(define plus-comm (for '(= (+ n m) (+ m n))))


#!eof
