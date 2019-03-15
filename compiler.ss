(define transpose
  (lambda (l*) (apply map list l*)))
(define eqp?
  (lambda (u) (lambda (v) (eq? u v))))

(define teq?
  ;; Compares two mk terms
  (lambda (t1 t2)
    (or (eq? t1 t2)
        (and (pair? t1) (pair? t2)
             (teq? (car t1) (car t2))
             (teq? (cdr t1) (cdr t2))))))
(define var (lambda (name scope) (vector name scope)))
(define var->name (lambda (var) (vector-ref var 0)))
(define var->scope (lambda (var) (vector-ref var 1)))
(define var? vector?)
(define var<?
  ;; v1 is prioritized over v2
  (lambda (v1 v2)
    (let ([n1 (var->name v1)] [s1 (var->scope v1)]
          [n2 (var->name v2)] [s2 (var->scope v2)])
      (or (< s1 s2)
          (string<? (symbol->string n1) (symbol->string n2))))))

(define-syntax case-term
  ;; A type dispatch for mk terms
  (syntax-rules ()
    [(_ e [v e1] [(a d) e2] [atom e3])
     (let ([term e])
       (cond
        [(var? term) (let ([v term]) e1)]
        [(pair? term) (let ([a (car term)] [d (cdr term)]) e2)]
        [else (let ([atom term]) e3)]))]))

;;; Associations (in substitutions)
(define make-s  (lambda (u v) `(,u ,v)))
(define lhs car)
(define rhs cadr)

;;; Constraint stuff
(define CONSTRAINTS '(S C D O))
(define empty-S '())
(define empty-D '())
(define empty-O '())
(define make-c (lambda (S C D O) (list S C D O)))
(define empty-c (make-c empty-S empty-D empty-O))
(define c->S (lambda (c) (c-> c 'S)))
(define c->C (lambda (c) (c-> c 'C)))
(define c->D (lambda (c) (c-> c 'D)))
(define c->O (lambda (c) (c-> c 'O)))
(define c->
  (lambda (c store)
    (rhs (assq store (transpose `(,CONSTRAINTS ,c))))))
(define update-S (lambda (c S) (make-c S (c->C c) (c->D c) (c->O c))))
(define update-C (lambda (c C) (make-c (c->S c) C (c->D c) (c->O c))))
(define update-D (lambda (c D) (make-c (c->S c) (c->C c) D (c->O c))))
(define update-O (lambda (c O) (make-c (c->S c) (c->C c) (c->D c) O)))

;;; Answer stream monad (actually just lists)
(define mzero (lambda () '()))
(define unit (lambda (x) `(,x)))
(define choice (lambda (x y) `(,x . ,y)))

(define-syntax lambdag@
  ;; Special lambda for goal constructor (with state inspection)
  (syntax-rules (:)
    [(_ (c) e) (lambda (c) e)]
    [(_ (c : s* ...) e)
     (lambda (c)
       (let ([s* (c-> c 's*)] ...)
         e))]))

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
     [else `(,(car S+) . ,(prefix-S (cdr S+) S))])))

(define unify
  (lambda (t1 t2 S)
    (let ([t1 (walk t1 S)]
          [t2 (walk t2 S)])
      (cond
       [(eq? t1 t2) S]
       [;; The more important variable will be on the right
        (and (var? t1) (var? t2))
        (cond
         [(var<? t1 t2) `(,(make-s t1 t2) . ,S)]
         [else `(,(make-s t2 t1) . ,S)])]
       [(var? t1) (ext-S t1 t2 S)]
       [(var? t2) (ext-S t2 t1 S)]
       [(and (pair? t1) (pair? t2))
        (let ([S+ (unify (car t1) (car t2) S)])
          (and S+ (unify (cdr t1) (cdr t2) S+)))]
       [(equal? t1 t2) S]
       [else #f]))))

(define ext-S
  (lambda (v t S)
    (and (not (occurs? v t S)) `(,(make-s v t) . ,S))))

(define occurs?
  (lambda (v t S)
    (let occurs? ([t t])
      (let ([t (walk t S)])
        (case-term t
          [u (eq? u v)]
          [(a d) (or (occurs? a) (occurs? d))]
          [atom #f])))))

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
           [(==fail? S+ D) (mzero)]
           [else (unit (update-S c S+))]))]
       [else (mzero)]))))

(define subsumed?
  ;; Is d subsumed by d*?
  (lambda (d d*)
    (cond
     [(null? d*) #f]
     [else (let ([d^ (unify* (car d*) d)])
             (or (and d^ (eq? d^ d))
                (subsumed? d (cdr d*))))])))
(define rem-subsumed
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
             [(null? pS) (mzero)]
             [else (unit (update-D c `(,pS . ,D)))])))]
       [else (unit c)]))))

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

;;; Goal constructors
(define succeed (lambdag@ (c) (unit c)))
(define fail    (lambdag@ (c) (mzero)))

(define conj2
  (lambda (g1 g2)
    (lambdag@ (c) (bind (g1 c) g2))))
(define bind
  (lambda (c* g) (apply append (map g c*))))

(define-syntax conj
  (syntax-rules ()
    [(_) succeed]
    [(_ g) g]
    [(_ g g* ...) (conj2 g (conj g* ...))]))

(define disj2
  (lambda (g1 g2)
    (lambdag@ (c) (append (g1 c) (g2 c)))))

(define-syntax disj
  (syntax-rules ()
    [(_) fail]
    [(_ g) g]
    [(_ g g* ...) (disj2 g (disj g* ...))]))

(define-syntax fresh
  (syntax-rules ()
    [(_ (x* ...) g g* ...)
     (lambdag@ (c : C)
       (let ([x* (var 'x* C)] ...)
         ((conj g g* ...) (update-C c (+ C 1)))))]))

(define-syntax conde
  (syntax-rules ()
    [(_ [g g* ...] ...)
     (disj (conj g g* ...) ...)]))

(define-syntax run*
  (syntax-rules ()
    [(_ (q0 q* ...) g g* ...)
     ((fresh (q0 q* ...)
        g g* ...
        (lambdag@ (final-c)
          (reify `(,q0 ,q* ...) final-c)))
      empty-c)]))

(define walk*
  (lambda (t S)
    (let ([t (walk t S)])
      (case-term t
        [v v]
        [(a d) `(,(walk* a S) . ,(walk* d S))]
        [a a]))))

(define purify
  (lambda (D)
    (filter (lambda (d) (not (or (constant? d) (has-iv? d)))) D)))
(define constant?
  (lambda (t)
    (case-term t
      [v #f]
      [(a d) (and (constant? a) (constant? d))]
      [atom #t])))
(define has-iv?
  (lambda (t)
    (case-term t
      [v (> (var->scope v) 0)]
      [(a d) (or (has-f? a) (has-f? d))]
      [atom #f])))

(define reify
  ;; This will return a c with clausal S
  (lambda (q* c)
    (let ([S (c->S c)] [D (c->D c)] [O (c->O c)])
      (let ([t (walk* q* S)] [D (walk* D S)] [O (walk* O S)])
        (let ([;; Gotta respect O
               R (reify-S `(,t ,O))])
          (let ([t (walk* t R)]
                [D (walk* D R)]
                [O (walk* O R)])
            (let ([D (rem-subsumed (purify D))])
              `(,t ,D ,O))))))))

(define reify-S
  ;; I'M HERE! WHAT AM I TO DO TO SIGNAL REIFIED NAME?
  (lambda (t)
    (let reify-S ([t t] [S empty-S])
      (case-term (walk t S)
        [v (cond
            [(eq? (var->scope v) 'f)
             (let ([new-var (var (length S) 'r)])
               `(,(make-s v new-var) . ,S))]
            [else S])]
        [(a d) (reify-S d (reify-S a S))]
        [a S]))))

(define fake
  (lambda (expr)
    (lambdag@ (c : O)
      (unit (update-O c `(,expr . ,O))))))


;;; Code generation techniques
(define-syntax run*au
  ;; run* with anti-unification analysis
  (syntax-rules ()
    [(_ (q0 q* ...) g g* ...)
     (let ([q0 (var 'q0 'q)] [q* (var 'q* 'q)] ...)
       (au-extract
        (map (lambda (c) (reify `(,q0 ,q* ...) c))
             ((conj g g* ...) empty-c))
        `(,q0 ,q* ...)))]))

(define au-extract
  (lambda (c* q*)
    (let ([t* (map car c*)]
          [D* (map cadr c*)]
          [O* (map caddr c*)])
      (let-values ([(au iS) (apply anti-unify t*)])
        (let ([auv* (map rhs iS)]
              [uS (;; Unification with au (common for all clauses)
                   unify q* au empty-S)]
              [;; Then each clause unify right back in (one for each clause)
               S* (map (lambda (v) (unify au v empty-S)) t*)])
          (let ([al0 (;; Can't rename queries
                      get-aliases uS q*)]
                [al* (map (lambda (S) (get-aliases S auv*)) S*)])
            (let ([uS (dedup (revars uS al0))])
              (let ([revars* (lambda (X*)
                               (map (lambda (X al)
                                      (revars X (append al0 al)))
                                    X* al*))])
                (let ([S* (map dedup (revars* S*))]
                      [D* (revars* D*)]
                      [O* (revars* O*)])
                  `(,uS ,(transpose `(,S* ,D* ,O*))))))))))))

(define sdup?
  (lambda (s) (eq? (lhs s) (rhs s))))
(define dedup
  (lambda (S)
    (filter (lambda (s) (not (sdup? s))) S)))

(define anti-unify
  ;; Returns the anti-unification and an inverse substitution
  (lambda t*
    (let anti-unify ([t* t*] [S '()])
      (cond
       [;; rule 7, for symbols only (alternative mentioned in the paper)
        ;; works for variables as well, since vars of different branches can still be eq?
        (for-all (lambda (t) (eq? (car t*) t)) (cdr t*))
        (values (car t*) S)]
       [;; rule 8
        (for-all pair? t*)
        (let*-values ([(aua S+) (anti-unify (map car t*) S)]
                      [(aud S++) (anti-unify (map cdr t*) S+)])
          (values `(,aua . ,aud) S++))]
       [;; rule 9 (cannot deal with variable shadowing)
        (find (lambda (s) (teq? (lhs s) t*)) S)
        =>
        (lambda (s) (values (rhs s) S))]
       [;; rule 10
        else
        (let ([new-var (var (length S) 'a)])
          (values new-var `(,(make-s t* new-var) . ,S)))]))))

(define get-aliases
  (lambda (S locked)
    (filter
     (lambda (s)
       (let ([l (lhs s)] [r (rhs s)])
         (and (var? l) (var? r)
            (var<? l r)
            (not (memq l locked)))))
     S)))

(define revars
  (lambda (t S)
    (let revars ([t t])
      (case-term t
        [v (let ([p (assq v S)])
             (if p (rhs p) v))]
        [(a d) `(,(revars a) . ,(revars d))]
        [atom atom]))))
