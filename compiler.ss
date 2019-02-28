(define empty-S '())
(define make-s  (lambda (u v) `(,u ,v)))
(define empty-D '())
(define empty-O '())
(define make-c (lambda (S D O) (list S D O)))
(define empty-c (make-c empty-S empty-D empty-O))
(define c->
  (lambda (c store)
    (cond
     [(eq? store 'S) (car c)]
     [(eq? store 'D) (cadr c)]
     [(eq? store 'O) (caddr c)])))
(define update-c
  (lambda (c store value)
    (let ([S (c-> c 'S)] [D (c-> c 'D)] [O (c-> c 'O)])
      (cond
       [(eq? store 'S) (make-c value D O)]
       [(eq? store 'D) (make-c S value O)]
       [(eq? store 'O) (make-c S D value)]))))

(define-syntax lambdag@
  (syntax-rules (:)
    [(_ (c) e) (lambda (c) e)]
    [(_ (c : s* ...) e)
     (lambda (c)
       (let ([s* (c-> c 's*)] ...)
         e))]))

(define lhs car)
(define rhs cadr)

(define var
  (lambda (name) (vector name)))
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
     [else `(,(car S+)
             . ,(prefix-S (cdr S+) S))])))

(define unify
  (lambda (u v S)
    (let ([u (walk u S)]
          [v (walk v S)])
      (cond
       [(eq? u v) S]
       [(var? u) (and (not (occurs? u v S)) `(,(make-s u v) . ,S))]
       [(var? v) (and (not (occurs? v u S)) `(,(make-s v u) . ,S))]
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

(define ==
  (lambda (u v)
    (lambdag@ (c : S D)
      (cond
       [(unify u v S) =>
        (lambda (S+)
          (cond
           [(==fail? S+ D) (list)]
           [else (list (update-c c 'S S+))]))]
       [else (list)]))))

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

(define =/=
  (lambda (u v)
    (lambdag@ (c : S D)
      (cond
       [(unify u v S) =>
        (lambda (S+)
          (let ([pS (prefix-S S+ S)])
            (cond
             [(null? pS) (list)]
             [else (list (update-c c 'D `(,pS . ,D)))])))]
       [else (list c)]))))

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

;;; Less core stuff
(define succeed (lambdag@ (c) (list c)))
(define fail    (lambdag@ (c) (list)))

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

(define rename
  (lambda (q* ans)
    (let ([v* (car ans)])
      (let rename ([q* q*] [v* v*] [ans ans])
        (cond
         [(null? q*) ans]
         [(var? (car v*)) (rename (cdr q*) (cdr v*)
                                  (substq (car q*) (car v*) ans))]
         [else (rename (cdr q*) (cdr v*) ans)])))))

(define-syntax run*
  (syntax-rules ()
    [(_ (q q* ...) g g* ...)
     (fresh (q q* ...)
       (map (lambda (c) (reify `(,q ,q* ...) c))
            ((conj g g* ...) empty-c)))]))

(define walk*
  (lambda (v S)
    (let ([v (walk v S)])
      (cond
       [(var? v) v]
       [(pair? v)
        `(,(walk* (car v) S) . ,(walk* (cdr v) S))]
       [else v]))))

(define reify-S
  ;; Generate numbered vars for anonymous variables instead of strings
  ;; Also handles non-vars, and duplications
  (lambda (v S)
    (let ([v (walk v S)])
      (cond
       [(var? v)
        (cond
         [(member v (map rhs S)) => (lambda _ S)]
         [else `((,v ,(var (length S))) . ,S)])]
       [(pair? v) (reify-S (cdr v) (reify-S (car v) S))]
       [else S]))))

(define purify
  (lambda (D r)
    (filter (lambda (d) (not (or (constant? d)
                         (any-useless-var? d r))))
            D)))

(define constant?
  (lambda (d)
    (cond
     [(var? d) #f]
     [(pair? d) (and (constant? (car d)) (constant? (cdr d)))]
     [else #t])))

(define any-useless-var?
  (lambda (v r)
    (cond
     [(var? v)
      (let ([v (walk v r)])
        (and (var? v)
           (not (number? (vector-ref v 0)))))]
     [(pair? v) (or (any-useless-var? (car v) r)
                   (any-useless-var? (cdr v) r))]
     [else #f])))

(define reify
  (lambda (q* c)
    (let ([S (c-> c 'S)]
          [D (c-> c 'D)]
          [O (c-> c 'O)])
      (let ([v (walk* q* S)]
            [D (walk* D S)]
            [O (walk* O S)])
        (let ([r (reify-S (list v O) empty-S)])
          (let ([v (walk* v r)]
                [D (walk* (rem-subsumed (purify D r)) r)]
                [O (walk* O r)])
            (rename q* `(,v ,D ,O))))))))

(define fake-goal
  (lambda (expr)
    (lambdag@ (c : S D O)
      (list (update-c c 'O expr)))))


;;; Code generation

(define anti-unify
  ;; Returns the anti-unification and an inverse substitution
  ;; Cannot deal with variable shadowing (for the use of 'assoc')
  (lambda t*
    (let anti-unify ([t* t*] [S '()])
      (cond
       [;; rule 7, for symbols only (alternative mentioned in the paper)
        (for-all (lambda (t) (eq? (car t*) t)) (cdr t*))
        (values (car t*) S)]
       [;; rule 8
        (for-all pair? t*)
        (let*-values ([(aua S+) (anti-unify (map car t*) S)]
                      [(aud S++) (anti-unify (map cdr t*) S+)])
          (values `(,aua . ,aud) S++))]
       [;; rule 9 (cannot deal with variable shadowing)
        (assoc t* S) => (lambda (s) (values (rhs s) S))]
       [;; rule 10
        else
        (let ([new-var (var (length S))])
          (values new-var `(,(make-s t* new-var) . ,S)))]))))

(define minimize-uni
  (lambda (answers)
    (let ([q* (map car answers)]
          [D* (map cadr answers)]
          [O* (map caddr answers)])
      (let-values ([(au S) (apply anti-unify q*)])
        (let ([S (map (lambda (s) `(,(car (car s)) . ,(cdr s))) S)])
          (S-process S))))))

(define extract-vars
  (lambda (t)
    (cond
     [(var? t) `(,t)]
     [(pair? t) (append (extract-vars (car t))
                        (extract-vars (cdr t)))]
     [else '()])))

(define S-process
  ;; The anti-unification variables are on the right
  (lambda (S)
    (cond
     [(null? S) '()]
     [else
      (append (let ([s (car S)])
                (let ([lhs (lhs s)]
                      [rhs (rhs s)])
                  (cond
                   [(pair? lhs)
                    (append
                     (map (lambda (v*) `(fresh-vars v*))
                          (extract-vars lhs))
                     `((== ,lhs ,rhs)))]
                   [else `((== ,lhs ,rhs))])))
              (S-process (cdr S)))])))
