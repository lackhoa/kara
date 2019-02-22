;; State :: Store X Counter
;; Goal :: State -> Stream

(define ccons
  (lambda (x) (lambda (y) (cons x y))))

(define empty-store
  '((== . ())
    (=/= . ())
    (not-pairo . ())
    (symbolo . ())
    (absento . ())))
(define update-store
  (lambda (store constraint value)
    (let* ([current-cell (car store)]
           [current-constraint (car current-cell)])
      (cond
       [(eq? current-constraint constraint)
        `((,constraint . ,value) . ,(cdr store))]
       [else `(,current-cell
               .
               ,(update-store constraint (cdr store) value))]))))
(define store-ref
  (lambda (store constraint)
    (cdr
     (cond
      [(eq? constraint '==) (car store)]
      [(eq? constraint '=/=) (cadr store)]
      [(eq? constraint 'not-pairo) (caddr store)]
      [(eq? constraint 'symbolo) (cadddr store)]
      [(eq? constraint 'absento) (car (cddddr store))]))))

(define-syntax lambdag@
  (syntax-rules (:)
    [(_ (s/c) e)
     (lambda (s/c) e)]
    [(_ (s c) e)
     (lambda (s/c)
       (let ([s (car s/c)] [c (cdr s/c)])
         e))]))

(define-syntax enforce-constraint
  (syntax-rules ()
    [(_ constraint arg*)
     (lambdag@ (s c)
       (let ([s+ (update-store s 'constraint (ccons arg*))])
         (return `(,s+ . ,c))))]))

(define ==
  (lambda (u v)
    (enforce-constraint '== `(,u . ,v))))

(define =/=
  (lambda (u v)
    (enforce-constraint '=/= `(,u . ,v))))

(define absento
  (lambda (u v)
    (enforce-constraint 'absento `(,u . ,v))))

(define not-pairo
  (lambda (u)
    (enforce-constraint 'not-pairo u)))

(define symbolo
  (lambda (u)
    (enforce-constraint 'symbolo u)))

(define return
  (lambdag@ (s/c)
    (if (bad? (car s/c)) '() (list s/c))))

(define bad?
  (lambda (st)
    (let ([== (store-ref st '==)])
      (cond
       ;; Note that s+ is recomputed every time
       ((good-s? ==)
        => (lambda (s+)
            (or ((bad-d? (store-ref st '=/=)) s+)
               ((bad-np? (store-ref st 'not-pairo)) s+)
               ((bad-sym? (store-ref st 'symbolo)) s+)
               ((bad-abs? (store-ref st 'absento)) s+))))
       (else #t)))))

(define bad-np?
  (lambda (np-store)
    (lambda (s)
      (ormap
       (lambda (t)
         (pair? (find t s)))
       np-store))))

(define bad-sym?
  (lambda (sym-store)
    (lambda (s)
      (ormap
       (lambda (t)
         (let ([t (find t s)])
           (and (not (symbol? t))
              (not (var? t)))))
       sym-store))))

(define bad-d?
  (lambda (d-store)
    (lambda (s))
    (ormap
     (lambda (pr)
       (equal? (unify (car pr) (cdr pr) s) s))
     d-store)))

(define mem?
  (lambda (u v s)
    (let ((v (find v s)))
      (or (equal? (unify u v s) s)
         (and (pair? v)
            (or (mem? u (car v) s)
               (mem? u (cdr v) s)))))))
(define bad-abs?
  (lambda (abs-store)
    (lambda (s)
      (ormap
       (lambda (pr)
         (mem? (car pr) (cdr pr) s))
       abs-store))))

;; Subst :: [(Var X Term)]

;; [(Term X Term)] -> Maybe Subst
(define good-s?
  ;; This is like doing every unification from start to end
  (lambda (s)
    (fold-right
     (lambda (pr s)
       (and s (unify (car pr) (cdr pr) s)))
     '()
     s)))

;; Term ::= Var | Symbol | Boolean | () | (Term . Term)

(define find
  ;; This is just "walk"
  (lambda (u s)
    (let ([pr (assv u s)])
      (if pr (find (cdr pr) s) u))))

(define unify
  (lambda (u v s)
    (let ((u (find u s)) (v (find v s)))
      (cond
       [(eqv? u v) s]
       [(var? u) `((,u . ,v) . ,s)]
       [(var? v) `((,v . ,u) . ,s)]
       [(and (pair? u) (pair? v))
        (let ((s (unify (car u) (car v) s)))
          (and s (unify (cdr u) (cdr v) s)))]
       [else #f]))))

;; We don't really care what the second argument is**

(define var (lambda (n) n))
(define var? number?)

(define call/fresh
  (lambda (f)
    (lambdag@ (s c)
      ((f (var c)) `(,s . ,(+ c 1))))))

;;** Caveat:
;; We should add the "occur? check", to retain soundness.

(define disj
  (lambda (g1 g2)
    (lambdag@ (s/c)
      ($append (g1 s/c) (g2 s/c)))))

(define-syntax delay/name
  ;; Hairy!
  (syntax-rules ()
    [(_ e) (lambda () e)]))
(define promise? procedure?)

(define $append
  (lambda ($1 $2)
    (cond
     [(null? $1) $2]
     [(promise? $1) (delay/name ($append $2 (force $1)))]
     [else `(,(car $1) . ,($append (cdr $1) $2))])))

(define conj
  (lambda (g1 g2)
    (lambdag@ (s/c)
      ($append-map g2 (g1 s/c)))))

(define $append-map
  (lambda (g $)
    (cond
     [(null? $) '()]
     [(promise? $) (delay/name ($append-map g (force $)))]
     [else ($append (g (car $)) ($append-map g (cdr $)))])))

(define-syntax defrel
  (syntax-rules ()
    [((_ defname . args) g)
     (define defname
       (lambda args
         (lambdag@ (s/c)
           (delay/name (g s/c)))))]))

;; Stream :: Mature | Immature
;; Mature :: () | State X Stream
;; Immature :: Unit -> Stream

(define pull
  (lambda ($)
    (if (promise? $) (pull (force $)) $)))

(define take
  (lambda (n $)
    (cond
     [(null? $) '()]
     [(and n (zero? (- n 1))) (list (car $))]
     [else `(,(car $)
             . ,(take (and n (- n 1)) (pull (cdr $))))])))

(define call/initial-state
  (lambda (n g)
    (take n (pull (g `(,empty-store . 0))))))

(defrel (rembero x ls o)
  (disj
   (conj (== ls '()) (== ls o))
   (call/fresh
    (lambda (a)
      (call/fresh
       (lambda (d)
         (conj
          (== `(,a . ,d) ls)
          (disj
           (conj
            (=/= a x)
            (call/fresh
             (lambda (res)
               (== `(,a . ,res) o)
               (rembero x d res))))
           (conj
            (== a x)
            (rembero x d o))))))))))
