#|Notes
Expressions:
* Compound formula:
++ Quantification: `(,forall ,Sym ,Formula)
++ Implication: `(-> ,Formula ,Formula)
* Atomic formula: [Sym . [Term]]
* Compound term: [Sym . [Term]]
* Var: Sym
|#

;;; Macros & helpers
(define for
  (lambda (f)
    (let loop ([vs (free-vars f)])
      (cond
       [(null? vs) f]
       [else `(forall ,(car vs) ,(loop (cdr vs)))]))))

(define free-vars
  (lambda (f)
    (dedup
     (let free-vars ([f f] [bound '()])
       (pmatch f
         [(forall ,v ,f)
          (free-vars f `(,v ,@bound))]
         [(-> ,f ,g)
          (,@(free-vars f bound)
           ,@(free-vars g bound))]
         [(? ,@args)
          (let ([free-vars
                 (lambda (t)
                   (pmatch t
                     [(,fun ,@args)
                      (apply append
                        (map (lambda (arg) (free-vars arg bound)) args))]
                     [,x
                      (if (memq x bound) `(,x) '())]))])
            (apply append
              (map (lambda (arg) (free-vars arg)) args)))])))))

(define substitute
  (lambda (x s f)
    (pmatch f
      [(forall ,y ,g)
       (cond
        [(eq? x y) f]
        [(memq y (free-vars s))
         (let ([y* (freshen (sy->str y)
                            `(,x
                              ,@(free-vars s)
                              ,@(free-vars g)))])
           (let ([g (substitute y y* g)])
             (let ([g (substitute x s g)])
               `(forall ,y* ,g))))]
        [else `(forall ,y ,(substitute x s g))])]
      [,y
       (guard (var? y))
       (if (eq? x y) s f)]
      [(,fun . ,args)
       (let ([sub (lambda (arg) (substitute x s arg))])
         `(,fun ,@(map sub args)))]
      [else f])))

(define inst
  (lambda (f x)
    (pmatch f
      [(forall ,y ,g) (substitute y x g)]
      [else (error 'inst "Not a forall formula" f)])))

;;; Definitions
(define-record ctx (v a g q))

(define init-env  (lambda (g) (make-ctx '() '() g '())))
(define done
  (lambda (ctx)
    (let ([q (ctx-q ctx)])
      (pmatch q
        [() #f]
        [((,v ,a ,g) . ,rest)
         (make-ctx v a g rest)]))))
(define local-a   (lambda (env) (ctx-a (local-ctx env))))
(define local-g   (lambda (env) (ctx-g (local-ctx env))))
(define local-v   (lambda (env) (ctx-g (local-ctx env))))
(define ref       (lambda (i env) (list-ref (local-a env) i)))

(define-syntax go
  (syntax-rules ()
          [(_) (lambda (x) x)]
          [(_ f f* ...) (lambda (x)
                          ((go f* ...) (f x)))]))

(define-syntax prove
  (syntax-rules ()
          [(_ g steps ...)
           (print-env ((go steps ...)
                       (init-env g)))]))

(define print-env
  (lambda (env)
    (if (null? env) "All done!"
        (begin
          (let ([a (local-a env)]
                [g (local-g env)])
            (pp "Assets:" )
            (for-each (lambda (a) (pp a)) a)
            (pp "Goal:" g))
          (let ([pg (lambda (ctx) (pp (ctx-g ctx)))])
            (pp "Queue:")
            (for-each pg (cdr env)))))))

;;; Tactics (inference rules)
(define clean
  (lambda (env)
    (let ([a (local-a env)]
          [g (local-g env)])
      (let ([test (lambda (f) (alpha-equiv? g f))])
        (if (exists test a) (done env) env)))))

(define mp
  (lambda (imp)
    (lambda (env)
      (let ([c (get-conse imp)]
            [g (local-g env)])
        (cond
         [(equal? c g)
          ((go (assert (get-conse imp))
               (gain imp))
           env)]
         [else
          (error 'mp "From _ we cannot derive _" imp g)])))))

(define free-vars-a (lambda (a) (apply append (map free-vars a))))

(define intro
  (lambda (env)
    (let ([a (local-a env)]
          [g (local-g env)])
      (pmatch g
        [(forall ,v ,f)
         (let ([v* (freshen (sy->str v)
                            (free-vars-a a))])
           `(,(make-ctx a (substitute v v* f))
             .
             ,(cdr env)))]
        [(-> ,ante ,conse)
         `(,(make-ctx `(,ante ,@a) conse)
           .
           ,(cdr env))]))))

(define intro*
  (lambda (env)
    (let ([g (local-g env)])
      (pmatch g
        [(forall ? ?) (intro* (intro env))]
        [(-> ? ?) (intro* (intro env))]
        [else env]))))

(define alpha-equiv?
  (lambda (e1 e2)
    (let alpha ([e1 e1] [e2 e2]
            [xs '()] [ys '()])
      (pmatch `(,e1 ,e2)
        [(,x ,y)
         (guard (var? x) (var? y))
         (pmatch `(,(assq x xs) ,(assq y ys))
           [(#f #f)           (eq? x y)]
           [((? ,b1) (? ,b2)) (eq? b1 b2)]
           [(? ?)             #f])]
        [((forall ,x ,b1) (forall ,y ,b2))
         (let ([fresh (gensym)])
           (let ([xs `((,x ,fresh) ,@xs)]
                 [ys `((,y ,fresh) ,@ys)])
             (alpha b1 b2 xs ys)))]
        [((,fun1 . ,args1) (,fun2 . ,args2))
         (and (eq? fun1 fun2)
              (= (length args1) (length args2))
              (let ([alpha (lambda (arg1 arg2) (alpha arg1 arg2 xs ys))])
                (andmap alpha args1 args2)))]
        [(,c1 ,c2) (eq? c1 c2)]))))

(define freshen
  (lambda (name used)
    ;; Str -> [Var] -> Var
    (let freshen ([name name])
      (cond
       [(memq (str->sy name) used)
        (freshen (str-app name "*"))]
       [else (str->sy name)]))))

(define gain
  ;; Gain a new formula for free
  (lambda (f)
    (lambda (env)
      (let ([a (local-a env)]
            [g (local-g env)])
        `(,(make-ctx `(,f ,@a) g)
          ,@(cdr env))))))

(define argue
  ;; Prove something for no reason
  (lambda (f)
    (lambda (env)
      (cond
       [(null? env) `(,(make-ctx '() f))]
       [else (let ([a (local-a env)])
               `(,@env ,(make-ctx a f)))]))))

(define assert
  ;; Prove it and thou shall get it
  (lambda (f) (go (argue f) (gain f))))

;;; Axioms
(define ind
  (lambda (env)
    (let ([a (local-a env)]
          [g (local-g env)])
      ((go
        (argue (inst g 0))
        (let ([x (freshen "N" (free-vars-a a))])
          (argue `(forall ,x (-> ,(inst g x)
                           ,(inst g `(s ,x))))))
        done)
       env))))

(define inj
  (lambda (env)
    (let ([g (local-g env)])
      (pmatch g
        [(= (,fun . ,args1) (,fun . ,args2))
         (let loop ([args1 args1] [args2 args2])
           (cond
            [(null? args1) (done env)]
            [else
             ((assert `(= ,(car args1) ,(car args2)))
              (loop (cdr args1) (cdr args2)))]))]
        [else (error 'inj "Tactic does not apply" g)]))))

(define rwl
  (lambda (l*)
    (lambda (env)
      (let ([g (local-g env)])
        (pmatch g
          [(= ,l ,r)
           ((go (assert `(= ,l* ,r))
                done)
            env)]
          [else (error 'rwl "Not an equality" g)])))))

(define rwr
  (lambda (r*)
    (lambda (env)
      (let ([g (local-g env)])
        (pmatch g
          [(= ,l ,r)
           ((go (assert `(= ,l ,r*))
                done)
            env)]
          [else (error 'rwr "Not an equality" g)])))))

(define refl
  (lambda (env)
    (let ([g (local-g env)])
      (pmatch g
        [(= ,f ,f) (done env)]
        [else (error 'refl "Not reflable" g)]))))

(define symm
  (lambda (env)
    (let ([g (local-g env)])
      (pmatch g
        [(= ,f ,h)
         ((go done (assert `(= ,h ,f)))
          env)]
        [else (error 'symm "Not an equality" g)]))))

;; Rewriting
(define pat-match
  (lambda (pat term)
    (let pat-match ([pat pat] [term term] [S '()])
      (cond
       [(var? pat)
        (pmatch (assq pat S)
          [(,pat ,t) (and (equal? term t) S)]
          [#f `((,pat ,term) . ,S)])]
       [(and (pair? pat) (pair? term))
        (let ([S (pat-match (car pat) (car term) S)])
          (and S (pat-match (cdr pat) (cdr term) S)))]
       [(eq? pat term) S]
       [else #f]))))

(define fill
  (lambda (pat S)
    (let fill ([pat pat])
      (cond
       [(var? pat)
        (pmatch (assq pat S)
          [(,pat ,res) res]
          [else (error 'fill "A weird situation!" pat S)])]
       [(pair? pat)
        `(,(fill (car pat)) ,@(fill (cdr pat)))]
       [else pat]))))

(define rewrite-outer
  (lambda (rule g)
    (pmatch rule
      [(= ,l ,r)
       (let ([S (pat-match l g)])
         (if S (fill r S) g))]
      [else (error 'rewrite-outer "Not a rewrite rule" rule)])))

(define rewrite-core
  (lambda (f g)
    (let ([rule (rip f)])
      (pmatch rule
        [(= ,l ,r)
         (let rewrite-core ([g g])
           (let ([g (rewrite-outer rule g)])
             (cond
              [(pair? g) `(,(rewrite-core (car g))
                           ,@(rewrite-core (cdr g)))]
              [else g])))]
        [else (error 'rewrite "Not a rewrite rule" rule)]))))

(define rewrite
  (lambda (f)
    (lambda (env)
      (let ([g (local-g env)])
        ((go (assert (rewrite-core f g))
             done)
         env)))))

(define rip (lambda (f) (pmatch f [(forall ,? ,g) (rip g)] [else f])))

;; Arithmetic
(define plus0  (for '(= (+ 0 N) N)))
(define plus-s (for '(= (+ (s N) M) (s (+ N M)))))
(define plus0r (for '(= (+ N 0) N)))
(define plus-comm (for '(= (+ N M) (+ M N))))


#!eof
