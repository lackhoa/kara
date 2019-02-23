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

(define ans list)  ;; [c], empty means failure
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
  (lambda (u v s)
    (let ((u (walk u s))
          (v (walk v s)))
      (cond
       [(eq? u v) s]
       [(var? u) (and (not (occurs? u v s)) `((,u . ,v) . ,s))]
       [(var? v) (and (not (occurs? v u s)) `((,v . ,u) . ,s))]
       [(and (pair? u) (pair? v))
        (let ((s (unify (car u) (car v) s)))
          (and s (unify (cdr u) (cdr v) s)))]
       [(equal? u v) s]
       [else #f]))))

(define occurs?
  (lambda (x v s)
    (let ([v (walk v s)])
      (cond
       [(var? v) (eq? v x)]
       [(pair? v)
        (or
         (occurs? x (car v) s)
         (occurs? x (cdr v) s))]
       [else #f]))))

(define unify*
  ;; This also returns prefix
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
            [else (ans (make-c S+ D))]))]
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
             [else (ans (make-c S `(,pS . ,D)))])))]
       [else (ans (make-c S D))]))))

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

(define conj2
  (lambda (g1 g2)
    (lambdag@ (c)
      (let ([c* (g1 c)])
        (apply append
          (map (lambda (c-new) (g2 c-new)) c*))))))
