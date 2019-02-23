(define make-c (lambda (S D) (list S D)))
(define empty-c (make-c '(#|S|#) '(#|D|#)))
(define c->S car)
(define c->D cadr)

(define-syntax lambdag@
  (syntax-rules (:)
    [(_ (c) e) (lambda (c) e)]
    [(_ (c : S D) e)
     (lambda (c)
       (let ([S (c->S c)] [D (c->D c)])
         e))]))

(define ans list)
(define lhs car)
(define rhs cdr)

(define var
  (lambda (name)
    (vector name)))

(define var? vector?)

(define walk
  (lambda (u S)
    (cond
     [(and (var? u) (assq u S)) =>
      (lambda (pr) (walk (rhs pr) S))]
     [else u])))

(define prefix-S
  (lambda (S+ S)
    (cond
     [(eq? S+ S) '()]
     [else (cons (car S+)
                 (prefix-S (cdr S+) S))])))

(define unify
  (lambda (u v s)
    (let ([u (walk u s)]
          [v (walk v s)])
      (cond
       [(eq? u v) s]
       [(var? u) (ext-s-check u v s)]
       [(var? v) (ext-s-check v u s)]
       [(and (pair? u) (pair? v))
        (let ((s (unify (car u) (car v) s)))
          (and s (unify (cdr u) (cdr v) s)))]
       [(equal? u v) s]
       [else #f]))))

(define occurs-check
  (lambda (x v s)
    (let ([v (walk v s)])
      (cond
       [(var? v) (eq? v x)]
       [(pair? v)
        (or
         (occurs-check x (car v) s)
         (occurs-check x (cdr v) s))]
       [else #f]))))

(define ext-s-check
  (lambda (x v s)
    (cond
     [(occurs-check x v s) #f]
     [else (cons `(,x . ,v) s)])))

(define unify*
  (lambda (S+ S)
    (unify (map lhs S+) (map rhs S+) S)))


(define walk*
  (lambda (v S)
    (let ((v (walk v S)))
      (cond
       [(var? v) v]
       [(pair? v)
        `(,(walk* (car v) S) . ,(walk* (cdr v) S))]
       [else v]))))

(define =/=
  (lambda (u v)
    (lambdag@ (c : S D)
      (cond
       [(unify u v S) =>
        (lambda (S0)
          (let ([pfx (prefix-S S0 S)])
            (cond
             [(null? pfx) (ans)]
             [else (ans (make-c S `(,pfx . ,D)))])))]
       [else c]))))

(define ==
  (lambda (u v)
    (lambdag@ (c : S D)
      (cond
       [(unify u v S) =>
        (lambda (S+)
          (cond
           [(==fail-check S+ D) (ans)]
           [else (ans (make-c S+ D))]))]
       [else (ans)]))))

(define ==fail-check
  (lambda (S D)
    (=/=-fail-check S D)))

(define =/=-fail-check
  (lambda (S D)
    (exists (d-fail-check S) D)))

(define d-fail-check
  (lambda (S)
    (lambda (d)
      (cond
       [(unify* d S) =>
	(lambda (S+) (eq? S+ S))]
       [else #f]))))

;;; Keep these here for reference
(define rem-subsumed
  (lambda (D)
    (let rem-subsumed ([D D] [d^* '()])
      (cond
       [(null? D) d^*]
       [(or (subsumed? (car D) (cdr D))
           (subsumed? (car D) d^*))
        (rem-subsumed (cdr D) d^*)]
       [else
        (rem-subsumed (cdr D) (cons (car D) d^*))]))))

(define subsumed?
  (lambda (d d*)
    (cond
     [(null? d*) #f]
     [else
      (let* ([d*a (car d*)]
             [d^ (unify* d*a d)])
        (or
         (and d^ (eq? d^ d))
         (subsumed? d (cdr d*))))])))
