(define transpose
  (lambda (l*) (apply map list l*)))
(define eqp?
  (lambda (u) (lambda (v) (eq? u v))))
(define TAGS '(f c r a q))

(define var (lambda (name tag) (vector name tag)))
(define var->name (lambda (var) (vector-ref var 0)))
(define var->tag (lambda (var) (vector-ref var 1)))
(define var? vector?)
(define tagged? (lambda (var tag) (eq? (var->tag var) tag)))
(define var<
  (lambda (v1 v2)
    (let ([t1 (var->tag v1)]
          [t2 (var->tag v2)])
      (memq t2 (memq t1 TAGS)))))

(define-syntax case-term
  (syntax-rules ()
    [(_ e [v e1] [(a d) e2] [atom e3])
     (let ([term e])
       (cond
        [(var? term) (let ([v term]) e1)]
        [(pair? term) (let ([a (car term)] [d (cdr term)]) e2)]
        [else (let ([atom term]) e3)]))]))

;;; A single substitution cell
(define make-s  (lambda (u v) `(,u ,v)))
(define lhs car)
(define rhs cadr)

;;; Constraint stuff
(define empty-S '())
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
(define c->S (lambda (c) (c-> c 'S)))
(define c->D (lambda (c) (c-> c 'D)))
(define c->O (lambda (c) (c-> c 'O)))
(define update-c
  (lambda (c store value)
    (let ([S (c->S c)] [D (c->D c)] [O (c->O c)])
      (cond
       [(eq? store 'S) (make-c value D O)]
       [(eq? store 'D) (make-c S value O)]
       [(eq? store 'O) (make-c S D value)]))))
(define update-S (lambda (c value) (update-c c 'S value)))
(define update-D (lambda (c value) (update-c c 'D value)))
(define update-O (lambda (c value) (update-c c 'O value)))

;;; Stream stuff
(define mzero (lambda () '()))
(define unit (lambda (x) `(,x)))
(define choice (lambda (x y) `(,x . ,y)))

(define-syntax lambdag@
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
  (lambda (u v S)
    (let ([u (walk u S)]
          [v (walk v S)])
      (cond
       [(eq? u v) S]
       [;; The more important variable will be on the right
        (and (var? u) (var? v))
        (cond
         [(var< u v) `(,(make-s u v) . ,S)]
         [else `(,(make-s v u) . ,S)])]
       [(var? u) (let ([v (walk v S)])
                   (and (not (occurs? (eqp? u) v))
                      `(,(make-s u v) . ,S)))]
       [(var? v) (let ([u (walk u S)])
                   (and (not (occurs? (eqp? v) u))
                      `(,(make-s v u) . ,S)))]
       [(and (pair? u) (pair? v))
        (let ([S+ (unify (car u) (car v) S)])
          (and S+ (unify (cdr u) (cdr v) S+)))]
       [(equal? u v) S]
       [else #f]))))

(define occurs?
  (lambda (p t)
    (let occurs? ([t t])
      (case-term t
        [v (p v)]
        [(a d) (or (occurs? a) (occurs? d))]
        [atom #f]))))

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

;;; Less core stuff
(define succeed (lambdag@ (c) (unit c)))
(define fail    (lambdag@ (c) (mzero)))

(define conj2
  (lambda (g1 g2)
    (lambdag@ (c)
      (bind (g1 c) g2))))
(define bind
  (lambda (c* g)
    (apply append (map g c*))))

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
     (let ([x* (var 'x* 'f)] ...)
       (conj g g* ...))]))

(define-syntax conde
  (syntax-rules ()
    [(_ [g g* ...] ...)
     (disj (conj g g* ...) ...)]))

(define-syntax run*
  (syntax-rules ()
    [(_ (q0 q* ...) g g* ...)
     (let ([q0 (var 'q0 'q)] [q* (var 'q* 'q)] ...)
       (map (lambda (c) (reify `(,q0 ,q* ...) c))
            ((conj g g* ...) empty-c)))]))

(define walk*
  (lambda (t S)
    (let ([t (walk t S)])
      (case-term t
        [v v]
        [(a d) `(,(walk* a S) . ,(walk* d S))]
        [a a]))))

(define purify
  (lambda (D)
    (filter (lambda (d)
              (not (or (constant? d)
                    (;; fresh variables don't matter
                     occurs? (lambda (v) (tagged? v 'f)) d))))
            D)))

(define constant?
  (lambda (t)
    (case-term t
      [v #f]
      [(a d) (and (constant? a) (constant? d))]
      [atom #t])))

(define reify
  ;; This will return a c
  (lambda (q* c)
    (let ([S (c->S c)] [D (c->D c)] [O (c->O c)])
      (let ([v (walk* q* S)] [D (walk* D S)] [O (walk* O S)])
        (let ([R (reify-S `(;; Gotta respect O
                            ,v ,O))])
          (let ([S (transpose `(,q* ,(walk* v R)))]
                [D (walk* D R)]
                [O (walk* O R)])
            (let ([S (dedup S)]
                  [D (rem-subsumed (;; This will remove 'f vars
                                    purify D))])
              `(,S ,D ,O))))))))

(define reify-S
  ;; The sole purpose is to rename 'f variables to 'r
  (lambda (t)
    (let reify-S ([t t] [S empty-S])
      (case-term (walk t S)
        [v (cond
            [;; Variable still fresh -> gotta reify
             (tagged? v 'f)
             (let ([new-var (var (length S) 'r)])
               `(,(make-s v new-var) . ,S))]
            [else S])]
        [(a d) (reify-S d (reify-S a S))]
        [a S]))))

(define fake-goal
  (lambda (expr)
    (lambdag@ (c : S D O)
      (unit (update-O c expr)))))


;;; Code generation techniques

(define-syntax run*min
  (syntax-rules ()
    [(_ (q0 q* ...) g g* ...)
     (let ([q0 (var 'q0 'q)] [q* (var 'q* 'q)] ...)
       (minimize (map (lambda (c) (reify `(,q0 ,q* ...) c))
                      ((conj g g* ...) empty-c))
                 `(,q0 ,q* ...)))]))

(define minimize
  (lambda (c* q*)
    (let ([S* (map car c*)]
          [D* (map cadr c*)]
          [O* (map caddr c*)])
      (let (;; Convert to clausal form
            [v* (map (lambda (S) (walk* q* S)) S*)])
        (let-values ([(au iS) (apply anti-unify v*)])
          (let ([auv* (map rhs iS)]
                [uS (;; Unification with au (common)
                     unify q* au empty-S)]
                [;; Then each clause unify right back in (separately)
                 S* (map (lambda (v) (unify au v empty-S)) v*)])
            (let ([al0 (get-aliases uS '())]
                  [al* (map (lambda (S) (get-aliases S auv*)) S*)])
              (let ([uS (dedup (revars uS al0))])
                (let ([revars* (lambda (X*)
                                 (map (lambda (X al)
                                        (revars X (append al0 al)))
                                      X* al*))])
                  (let ([S* (map dedup (revars* S*))]
                        [D* (revars* D*)]
                        [O* (revars* O*)])
                    `(,uS ,(transpose `(,S* ,D* ,O*)))))))))))))

(define sdup?
  (lambda (s) (eq? (lhs s) (rhs s))))
(define dedup
  (lambda (S) (dedup-seen S '())))
(define teq?
  (lambda (t1 t2)
    (or (eq? t1 t2)
       (and (pair? t1) (pair? t2)
          (teq? (car t1) (car t2))
          (teq? (cdr t1) (cdr t2))))))
(define dedup-seen
  (lambda (S seen)
    (cond
     [(null? S) '()]
     [else
      (let ([s (car S)])
        (cond
         [(or (sdup? s) (find (lambda (s^) (teq? s s^)) seen))
          (dedup-seen (cdr S) seen)]
         [else
          `(,s . ,(dedup-seen (cdr S) `(,s . ,seen)))]))])))

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
            (var< l r)
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
