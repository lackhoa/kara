(define pmap
  (lambda (f pair)
    `(,(f (car pair)) . ,(f (cdr pair)))))

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

;; (define-syntax lambdat@
;;   (syntax-rules (:)
;;     [(_ )]))

(define lhs car)
(define rhs cadr)

(define var
  (lambda (name) (vector name)))
(define var->name
  (lambda (var) (vector-ref var 0)))
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

(define revars
  (lambda (v S)
    (cond
     [(and (var? v) (assq v S)) => rhs]
     [(pair? v) `(,(revars (car v) S) . ,(revars (cdr v) S))]
     [else v])))

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
     [(var? v) (not (numbered-var? (walk v r)))]
     [(pair? v) (or (any-useless-var? (car v) r)
                   (any-useless-var? (cdr v) r))]
     [else #f])))

(define reify
  (lambda (q* c)
    (let ([S (c-> c 'S)]
          [D (c-> c 'D)]
          [O (c-> c 'O)])
      (let ([v* (walk* q* S)]
            [D (walk* D S)]
            [O (walk* O S)])
        (let ([r (reify-S (list v* O) empty-S)])
          (let ([v* (walk* v* r)]
                [D (walk* (rem-subsumed (purify D r)) r)]
                [O (walk* O r)])
            (revars `(,v* ,D ,O)
                    (filter (lambda (vq) (var? (lhs vq)))
                            (transpose v* q*)))))))))

(define fake-goal
  (lambda (expr)
    (lambdag@ (c : S D O)
      (list (update-c c 'O expr)))))


;;; Code generation techniques
(define-syntax run*min
  (syntax-rules ()
    [(_ (q q* ...) g g* ...)
     (fresh (q q* ...)
       (minimize (map (lambda (c) (reify `(,q ,q* ...) c))
                      ((conj g g* ...) empty-c))
                 `(,q ,q* ...)))]))

(trace-define extract-vars
  (lambda (t)
    (cond
     [(var? t) `(,t)]
     [(pair? t) (append (extract-vars (car t))
                        (extract-vars (cdr t)))]
     [else '()])))
(define minimize
  (lambda (answers q*)
    (let ([ta (apply transpose answers)])
      (let ([v* (car ta)]
            [D* (cadr ta)]
            [O* (caddr ta)])
        (let-values ([(au iS) (apply anti-unify v*)])
          (let ([S* (extract-iS iS)]
                [uS (;; Unification with au is mandatory
                     transpose q* au)])
            (let ([r (renames uS q* '())])
              (let ([uS (dedup (revars uS r))])
                `(,uS
                  .
                  ,(;; Other unifications
                    map (lambda (SDO)
                          (let ([S (car SDO)]
                                [DO (cdr SDO)])
                            (let ([auv* ;; (map rhs iS) WRONG
                                   (extract-vars (map rhs uS))])
                              (let ([r (renames S (append auv* q*) auv*)])
                                (let ([S (dedup (revars S r))]
                                      [DO (revars DO r)])
                                  `(,S . ,DO))))))
                        (transpose S* D* O*)))))))))))

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
        (let ([new-var (var (au-name (length S)))])
          (values new-var `(,(make-s t* new-var) . ,S)))]))))

(define au-name
  (lambda (n)
    (string->symbol (string-append "au" (number->string n)))))

(define transpose
  (lambda l*
    (apply map list l*)))

(define extract-iS
  (lambda (iS)
    (let ([lhs* (map lhs iS)]
          [rhs* (map rhs iS)])
      (map (lambda (lhs) (transpose lhs rhs*))
           (apply transpose lhs*)))))

(define numbered-var?
  (lambda (v) (and (var? v) (number? (var->name v)))))

(define renames
  ;; Input: an S
  ;; Returns: a rename list
  (lambda (S freshes locked)
    (let ([S (filter (lambda (s) (andmap var? s)) S)])
      (filter
       (lambda (s) s)
       (map (lambda (s)
              (let ([lhs (lhs s)] [rhs (rhs s)])
                (cond
                 [(and (not (memq lhs freshes))
                     (not (memq lhs locked)))
                  s]
                 [(and (not (memq rhs freshes))
                     (not (memq rhs locked)))
                  (make-s rhs lhs)]
                 [(and (left-of lhs rhs freshes)
                     (not (memq lhs locked)))
                  (make-s lhs rhs)]
                 [(and (left-of rhs lhs freshes)
                     (not (memq rhs locked)))
                  (make-s rhs lhs)]
                 [else #f])))
            S)))))

(define left-of
  (lambda (v1 v2 f)
    (let left-of ([f f])
      (cond
       [(null? f) #f]
       [(eq? v1 (car f)) #t]
       [(eq? v2 (car f)) #f]
       [else (left-of (cdr f))]))))
