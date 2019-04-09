#|Notes
Term structure
+ Variables: uppcase symbols
+ Quantified formulae: 3-list whose first element is a forall
+ Functions: list whose first element is not a forall
+ Constants: Any not-uppercase symbol, number, null
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
         [,v
          (guard (var? v))
          (if (memq v bound) '() `(,v))]
         [(forall ,v ,f)
          (free-vars f `(,v ,@bound))]
         [(? . ,a*)
          (apply append
            (map (lambda (a) (free-vars a bound)) a*))]
         [else '()])))))

(define substitute
  (lambda (x s f)
    (cond
     [(not (var? x))
      (error 'substitute "Substituting non-var" x)]
     [else
      (pmatch f
        [,y
         (guard (var? y))
         (if (eq? x y) s f)]
        [(forall ,y ,g)
         (cond
          [(eq? x y) f]
          [(memq y (free-vars s))
           (let ([y* (freshen y `(,x
                                  ,@(free-vars s)
                                  ,@(free-vars g)))])
             (let ([g (substitute y y* g)])
               (let ([g (substitute x s g)])
                 `(forall ,y* ,g))))]
          [else `(forall ,y ,(substitute x s g))])]
        [(,fun . ,args)
         (let ([sub (lambda (arg) (substitute x s arg))])
           `(,fun ,@(map sub args)))]
        [else f])])))

(define inst
  (lambda (x f)
    (pmatch f
      [(forall ,y ,g) (substitute y x g)]
      [else (error 'inst "Not a universally quantified formula" f)])))

;;; Definitions
(define-record ctx (a g))

(define var?
  (lambda (t)
    (and (symbol? t)
         (char-upper-case? (string-ref (sy->str t) 0)))))

(define init-env  (lambda (g) `(,(make-ctx '() g))))
(define local-ctx (lambda (env) (car env)))
(define done      (lambda (env) (cdr env)))
(define local-a   (lambda (env) (ctx-a (local-ctx env))))
(define local-g   (lambda (env) (ctx-g (local-ctx env))))
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
        (print-ctx (local-ctx env)))))

(define print-ctx
  (lambda (ctx)
    `((Assets: ,@(ctx-a ctx))
      (Goal:   ,(ctx-g ctx)))))

;;; Inference rules (actions)
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

(define free-vars-a (lambda (a) (apply append free-vars a)))

(define intro
  (lambda (env)
    (let ([a (local-a env)]
          [g (local-g env)])
      (pmatch g
        [(forall ,v ,f)
         (let ([v* (freshen v (free-vars-a a))])
           `(,(make-ctx a (substitute v v* f))
             .
             ,(cdr env)))]
        [(-> ,ante ,conse)
         `(,(make-ctx `(,ante ,@a) conse)
           .
           ,(cdr env))]))))

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
    (let freshen ([name name])
      (cond
       [(memq name used)
        (freshen (str->sy (str-app (sy->str name) "*")))]
       [else name]))))

(define gain
  ;; Just gain a new formula (also clean)
  (lambda (f)
    (lambda (env)
      (let ([a (local-a env)]
            [g (local-g env)])
        (clean
         `(,(make-ctx `(,f ,@a) g)
           .
           ,(cdr env)))))))

(define assert
  ;; Prove it and thou shall get it
  (lambda (f)
    (lambda (env)
      (let ([a (local-a env)])
        (let ([env ((gain f) env)])
          (append env `(,(make-ctx a f))))))))

;; Capture-avoiding substitution!

;;; Axioms
(define ind
  (lambda (env)
    (let ([a (local-a env)]
          [g (local-g env)])
      ((go
        (assert (inst g 0))
        (let ([x (freshen 'N (free-vars-a a))])
          (assert `(forall ,x (-> ,(inst g x)
                            ,(inst g `(s ,x)))))))
       env))))

(define arity-table '([s 1] [+ 2]))

(define rwr
  (lambda (r*)
    (lambda (env)
      (let ([g (local-g env)])
        (pmatch g
          [(= ,l ,r)
           ((go (gain `(= ,l ,r))
                (assert `(= ,r ,r*))
                (assert `(= ,l ,r*)))
            env)]
          [else (error 'rwr "Not an equality" g)])))))

(define rwl
  (lambda (l*)
    (lambda (env)
      (let ([g (local-g env)])
        (pmatch g
          [(= ,l ,r)
           ((go (gain `(= ,l ,r))
                (assert `(= ,l ,l*))
                (assert `(= ,l* ,r)))
            env)]
          [else (error 'rwl "Not an equality" g)])))))

(define refl
  (lambda (env)
    (let ([g (local-g env)])
      (pmatch g
        [(= ,f ,f) (done env)]
        [else (error 'refl "Not reflable" g)]))))

(define sym
  (lambda (env)
    (let ([g (local-g env)])
      (pmatch g
        [(= ,f ,h)
         ((go done (assert `(= ,h ,f)))
          env)]
        [else (error 'sym "Not an equality" g)]))))

;; Equality

;; Arithmetic
(define plus0  (for '(= (+ 0 X) X)))
(define plus-S (for '(= (+ (s X) Y) (s (+ X Y)))))
(define plus0r (for '(= (+ X 0) X)))


#!eof
