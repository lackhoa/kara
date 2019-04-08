#|Notes
|#

;;; Macros & helpers
(define for
  (lambda (f)
    (let ([vs (dedup (free-vars f))])
      (let loop ([vs vs])
        (cond
         [(null? vs) f]
         [else `(forall ,(car vs) ,(loop (cdr vs)))])))))

(define free-vars
  (lambda (f)
    (let free-vars ([f f] [bound '()])
      (pmatch f
        [,v
         (guard (var? v))
         (if (memq v bound) `(,v) '())]
        [(forall ,v ,f)
         (free-vars f `(,v . ,bound))]
        [(,a . ,d)
         (append (free-vars a bound)
                 (free-vars d bound))]
        [else '()]))))

;;; Definitions
(define-record ctx (a g))

(define var?
  (lambda (t)
    (and (symbol? t)
         (char-upper-case? (string-ref (sy->str t) 0)))))

(define init-env  (lambda (g) `(,(make-ctx '() g))))
(define done cdr) ;; Dismiss local context
(define local-ctx (lambda (env) (car env)))
(define current-a (lambda (env) (ctx-a (local-ctx env))))
(define current-g (lambda (env) (ctx-g (local-ctx env))))
(define ref       (lambda (i env) (list-ref (current-a env) i)))

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

(define print-env (lambda (env) (print-ctx (local-ctx env))))

(define print-ctx
  (lambda (ctx)
    `((Assets: . ,(ctx-a ctx))
      (Goal:     ,(ctx-g ctx)))))

;;; Inference rules (actions)
(define clean
  (lambda (env)
    (let ([a (current-a env)]
          [g (current-g env)])
      (if (member g a) (cdr env) env))))

(define mp
  (lambda (imp)
    (lambda (env)
      (let ([c (get-conse imp)]
            [g (current-g env)])
        (cond
         [(equal? c g)
          ((go (assert (get-conse imp))
               (gain imp))
           env)]
         [else
          (error) 'mp1 "Can\'t have that, love!"])))))

(define intro
  (lambda (env)
    (let ([a (current-a env)]
          [g (current-g env)])
      (pmatch f
        [(forall ,v ,f)
         (let ([u (freshen v (free-vars a))])
           (gain (subst v u f) env))]
        [(-> ,ante ,conse)
         `(,(make-ctx `(,ante . ,a) conse)
           .
           ,(cdr env))]))))

(define subst
  (lambda (x s f)
    (pmatch f
      [,y
       (guard (var? y))
       (if (eq? x y) s f)]

      [(forall ,y ,g)
       (cond
        [(eq? x y) f]
        [(memq y (free-vars s))
         (let ([z (freshen y `(,x ,@(free-vars s) ,@(free-vars g)))])
           (let ([g (subst y z g)])
             (let ([g (subst x s g)])
               `(forall ,z ,g))))]
        [else `(forall ,y ,(subst x s g))])]

      [(,a . ,d)
       `(,(subst x s a) . ,(subst x s d))]

      [else f])))

(define freshen
  (lambda (name used)
    (let freshen ([name name])
      (cond
       [(memq name vs)
        (freshen (str->sy (str-app (sy->str name) "*")))]
       [else name]))))

(define gain
  ;; Just gain a new formula
  (lambda (f)
    (lambda (env)
      (let ([a (current-a env)]
            [g (current-g env)])
        `(,(make-ctx `(,f . ,a) g)
          .
          ,(cdr env))))))

(define assert
  ;; Prove it and thou shall get it
  (lambda (f)
    (lambda (env)
      (let ([a (current-a env)])
        (let ([env (gain f env)])
          (append env `(,(make-ctx a f))))))))

;; Capture-avoiding substitution!

;;; Axioms
(define ind
  (lambda (env)
    (let ([g (current-g env)])
      ((go
        (assert (inst g 0))
        (assert (let ([v (freshen 'M (free-vars g))])
                  `(forall ,v (-> ,(inst g v)
                            ,(inst g `(s ,v)))))))
       env))))

(define arity-table '([S 1] [+ 2]))

(define rwr
  (lambda (r*)
    (lambda (env)
      (let ([g (current-g env)])
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
      (let ([g (current-g env)])
        (pmatch g
          [(= ,l ,r)
           ((go (gain `(= ,l ,r))
                (assert `(= ,l ,l*))
                (assert `(= ,l* ,r)))
            env)]
          [else (error 'rwl "Not an equality" g)])))))

(define refl
  (lambda (env)
    (let ([g (current-g env)])
      (pmatch g
        [(= ,f ,f) (done env)]
        [else (error 'refl "Not reflable" g)]))))

(define sym
  (lambda (env)
    (let ([g (current-g env)])
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
