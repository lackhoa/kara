(define make-c (lambda (S D O) (list S D O)))
(define empty-c (make-c '(#|S|#) '(#|D|#) '(#|O|#)))
(define c->S car)
(define c->D cadr)
(define c->O caddr)
(define update-c
  (lambda (c store value)
    (let ([S (c->S c)] [D (c->D c)] [O (c->O c)])
      (cond
       [(eq? store 'S) (make-c value D O)]
       [(eq? store 'D) (make-c S value O)]
       [(eq? store 'O) (make-c S D value)]))))

(define-syntax lambdag@
  (syntax-rules (:)
    [(_ (c) e) (lambda (c) e)]
    [(_ (c : S D) e)
     (lambda (c)
       (let ([S (c->S c)] [D (c->D c)])
         e))]
    [(_ (c : S D O) e)
     (lambda (c)
       (let ([S (c->S c)] [D (c->D c)] [O (c->O c)])
         e))]))

(define ans list)  ;; [c], empty = failure
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
     ((eq? S+ S) '())
     (else (cons (car S+)
                 (prefix-S (cdr S+) S))))))

(define unify
  (lambda (u v S)
    (let ((u (walk u S))
          (v (walk v S)))
      (cond
       [(eq? u v) S]
       [(var? u) (and (not (occurs? u v S)) `((,u . ,v) . ,S))]
       [(var? v) (and (not (occurs? v u S)) `((,v . ,u) . ,S))]
       [(and (pair? u) (pair? v))
        (let ([S+ (unify (car u) (car v) S)])
          (and S+ (unify (cdr u) (cdr v) S+)))]
       [(equal? u v) S]
       [else #f]))))

(define occurs?
  (lambda (x v S)
    (let ([v (walk v S)])
      (cond
       [(var? v) (eq? v x)]
       [(pair? v)
        (or
         (occurs? x (car v) S)
         (occurs? x (cdr v) S))]
       [else #f]))))

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

(define-syntax ==
  (syntax-rules ()
    [(_ u v)
     (lambdag@ (c : S D)
       (cond
        [(unify u v S) =>
         (lambda (S+)
           (cond
            [(==fail? S+ D) (ans)]
            [else (ans (update-c c 'S S+))]))]
        [else (ans)]))]))

(define =/=
  (lambda (u v)
    (lambdag@ (c : S D)
      (cond
       [(unify u v S) =>
        (lambda (S+)
          (let ([pS (prefix-S S+ S)])
            (cond
             [(null? pS) (ans)]
             [else (ans (update-c c 'D `(,pS . ,D)))])))]
       [else (ans c)]))))

(define ==fail?
  (lambda (S D)
    (=/=-fail? S D)))

(define =/=-fail?
  (lambda (S D)
    (exists (d-fail? S) D)))

(define d-fail?
  (lambda (S)
    (lambda (d)
      (cond
       [(unify* d S) =>
	(lambda (S+) (null? (prefix-S S+ S)))]
       [else #f]))))

(define subsumed?
  ;; Is d subsumed by d*?
  (lambda (d d*)
    (cond
     [(null? d*) #f]
     [else (let ([d^ (unify* (car d*) d)])
             (or (and d^ (eq? d^ d))
                (subsumed? d (cdr d*))))])))
(define rem-subsumed
  ;; A conjunction of =='s is equivalent to one == on lists
  (lambda (D)
    (let rem-subsumed ([D D] [d^* '()])
      (cond
       [(null? D) d^*]
       [(or (;; Is is subsumed by the unprocessed list?
            subsumed? (car D) (cdr D))
           (;; Is it subsumed the processed list?
            subsumed? (car D) d^*))
        (rem-subsumed (cdr D) d^*)]
       [else
        (rem-subsumed (cdr D) `(,(car D) . ,d^*))]))))

;;; Less core stuff
(define succeed (== #t #t))
(define fail (== #t #f))

(define conj2
  (lambda (g1 g2)
    (lambdag@ (c)
      (let ([c* (g1 c)])
        (apply append
          (map (lambda (c-new) (g2 c-new)) c*))))))

(define-syntax conj
  (syntax-rules ()
    [(_) succeed]
    [(_ g) g]
    [(_ g g* ...) (conj2 g (conj g* ...))]))

(define disj2
  (lambda (g1 g2)
    (lambdag@ (c)
      (append (g1 c) (g2 c)))))

(define-syntax disj
  (syntax-rules ()
    [(_) fail]
    [(_ g) g]
    [(_ g g* ...) (disj2 g (disj g* ...))]))

(define-syntax fresh
  (syntax-rules ()
    [(_ (x* ...) g g* ...)
     (let ([x* (var 'x*)] ...)
       (conj g g* ...))]))

(define-syntax conde
  (syntax-rules ()
    [(_ [g g* ...] ...)
     (disj (conj g g* ...) ...)]))

(define-syntax run
  (syntax-rules ()
    [(_ g* ...)
     (let ([c* ((conj g* ...) empty-c)])
       (map (lambda (c)
              (update-c c 'D (rem-subsumed (c->D c))))
            c*))]))
