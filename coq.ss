#|Notes
|#

;;; Macros & helpers
(define for
  (lambda (f)
    (let ([vs (dedup (all-vars f))])
      (let loop ([vs vs])
        (cond
         [(null? vs) f]
         [else `(forall ,(car vs) ,(loop (cdr vs)))])))))

(define all-vars
  (lambda (f)
    (cond
     [(var? f) `(,f)]
     [(pair? f) (append (all-vars (car f))
                        (all-vars (cdr f)))]
     [else '()])))

;;; Definitions
(define-record ctx (a g))

(define var?
  (lambda (t)
    (and (symbol? t)
         (char-upper-case? (string-ref (sy->str t) 0)))))

(define init-env  (lambda (g) `(,(make-ctx '() g))))
(define local-ctx (lambda (env) (car env)))
(define current-a (lambda (env) (ctx-a (local-ctx env))))
(define current-g (lambda (env) (ctx-g (local-ctx env))))
(define ref (lambda (i env) (list-ref (current-a env) i)))

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
(define mp
  (lambda (imp ante)
    (lambda (env)
      (pmatch imp
        [(-> ,ante^ ,conse)
         (guard (equal? ante ante^))
         (gain conse env)]))))

(define intro
  (lambda (env)
    (let ([a (current-a env)]
          [g (current-g env)])
      (pmatch f
        [(forall ,X ,f)
         (let ([Y (freshen X env)])
           (gain (subst X Y f) env))]
        [(-> ,ante ,conse)
         `(,(make-ctx `(,ante . ,a) conse)
           .
           ,(cdr env))]))))

(define freshen
  (lambda (name env)
    (let ([t `(,(current-g env) . ,(current-a env))])
      (let ([vs (all-vars t)])
        (let loop ([name name])
          (cond
           [(memq name vs)
            (loop (str->sy (str-app (sy->str name) "*")))]
           [else name]))))))

(define gain
  ;; Just gain a new formula
  (lambda (f env)
    (let ([a (current-a env)]
          [g (current-g env)])
      `(,(make-ctx `(,f . ,a) g) . ,(cdr env)))))

(define assert
  ;; Prove it, then thou shall get it
  (lambda (f env)
    (let ([a (current-a env)]
          [g (current-g env)])
      `(,(make-ctx a f) . ,(gain f env)))))

;;; Axioms
(define ind
  ;; schema: state -> formulae
  (lambda (v P)
    (-> (inst P v 0)
       (-> `(forall M (-> ,(inst P v 'M)
                   ,(inst P v '(s M))))
          P))))

(define arity-table '([S 1] [+ 2]))

;; Equality
(define refl (for '(= X X)))
(define rwl  (for '(-> (= L R) (-> (= L L*) (= L* R)))))
(define rwr  (for '(-> (= L R) (-> (= R R*) (= L R*)))))
(define =sym (for '(-> (= X Y) (= Y X))))

;; Arithmetic
(define plus0  (for '(= (+ 0 X) X)))
(define plus-S (for '(= (+ (s X) Y) (s (+ X Y)))))
(define plus0r (for '(= (+ X 0) X)))


#!eof
