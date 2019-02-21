(define var (lambda (x) (vector x)))
(define var? vector?)

(define empty-s '())

(define walk
  (lambda (v s)
    (let ([a (and (var? v) (assv v s))])
      (cond
       [(pair? a) (walk (cdr a) s)]
       [else v]))))

(define ext-s
  (lambda (x v s)
    (cond
     [(occurs? x v s) #f]
     [else (cons `(,x . ,v) s)])))

(define occurs?
  (lambda (x v s)
    (let ([v (walk v s)])
      (cond
       [(var? v) (eqv? v x)]
       [(pair? v)
        (or (occurs? x (car v) s)
           (occurs? x (cdr v) s))]
       [else #f]))))

(define unify
  (lambda (u v s)
    (let ([u (walk u s)] [v (walk v s)])
      (cond
       [(eqv? u v) s]
       [(var? u) (ext-s u v s)]
       [(var? v) (ext-s v u s)]
       [(and (pair? u) (pair? v))
        (let ([s (unify (car u) (car v) s)])
          (and s
             (unify (cdr u) (cdr v) s)))]
       [else #f]))))

(define ==
  (lambda (u v)
    (lambda (s)
      (let ([s (unify u v s)])
        (if s `(,s) '())))))

(define succeed (lambda (s) `(,s)))

(define fail (lambda (s) '()))

(define disj2
  (lambda (g1 g2)
    (lambda (s)
      (append-inf (g1 s) (g2 s)))))

(define append-inf
  (lambda (s-inf t-inf)
    (cond
     [(null? s-inf) t-inf]
     [(pair? s-inf)
      (cons (car s-inf)
            (append-inf (cdr s-inf) t-inf))]
     [else (lambda ()
             (append-inf t-inf (s-inf)))])))

(define take-inf
  (lambda (n s-inf)
    (cond
     [(and n (zero? n)) '()]
     [(null? s-inf) '()]
     [(pair? s-inf)
      (cons (car s-inf)
            (take-inf (and n (sub1 n))
                      (cdr s-inf)))]
     [else (take-inf n (s-inf))])))

(define conj2
  (lambda (g1 g2)
    (lambda (s)
      (append-map-inf g2 (g1 s)))))

(define append-map-inf
  (lambda (g s-inf)
    (cond
     [(null? s-inf) '()]
     [(pair? s-inf)
      (append-inf (g (car s-inf))
                  (append-map-inf g (cdr s-inf)))]
     [else (lambda ()
             (append-map-inf g (s-inf)))])))

(define call/fresh
  (lambda (name f)
    (f (var name))))

(define reify-name
  (lambda (n)
    (string->symbol
     (string-append "_"
                    (number->string n)))))

(define walk*
  (lambda (v s)
    (let ([v (walk v s)])
      (cond
       [(var? v) v]
       [(pair? v)
        (cons
         (walk* (car v) s)
         (walk* (cdr v) s))]
       [else v]))))


(define reify-s
  (lambda (v r)
    (let ([v (walk v r)])
      (cond
       [(var? v)
        (let ([n (length r)])
          (let ([rn (reify-name n)])
            (cons `(,v . ,rn) r)))]
       [(pair? v)
        (let ([r (reify-s (car v) r)])
          (reify-s (cdr v) r))]
       [else r]))))

(define reify
  (lambda (v)
    (lambda (s)
      (let* ([v (walk* v s)]
             [r (reify-s v empty-s)])
        (walk* v r)))))

(define run-goal
  (lambda (n g)
    (take-inf n (g empty-s))))

(define once
  (lambda (g)
    (lambda (s)
      (let loop ([s-inf (g s)])
        (cond
         [(null? s-inf) '()]
         [(pair? s-inf)
          (cons (car s-inf) '())]
         [else (lambda ()
                 (loop (s-inf)))])))))


;;; Macros to lift it to miniKanren

(define-syntax disj
  (syntax-rules ()
    [(disj) fail]
    [(disj g) g]
    [(disj g0 g ...) (disj2 g0 (disj g ...))]))

(define-syntax conj
  (syntax-rules ()
    [(conj) succeed]
    [(conj g) g]
    [(conj g0 g ...) (conj2 g0 (conj g ...))]))

(define-syntax defrel
  (syntax-rules ()
    [(defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (s)
         (lambda ()
           ((conj g ...) s))))]))

(define-syntax run
  (syntax-rules ()
    [(run n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...)
                (== `(,x0 ,x ...) q) g ...))]
    [(run n q g ...)
     (let ([q (var 'q)])
       (map (reify q)
            (run-goal n (conj g ...))))]))

(define-syntax run*
  (syntax-rules ()
    [(run* q g ...) (run #f q g ...)]))

(define-syntax fresh
  (syntax-rules ()
    [(fresh () g ...) (conj g ...)]
    [(fresh (x0 x ...) g ...)
     (call/fresh 'x_0
                 (lambda (x0)
                   (fresh (x ...) g ...)))]))

(define-syntax conde
  (syntax-rules ()
    [(conde (g ...) ...)
     (disj (conj g ...) ...)]))
