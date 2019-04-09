;;; Functions
;; pmatch
;; (pmatch exp <clause> ...[<else-clause>])
;; <clause> ::= (<pattern> <guard> exp ...)
;; <else-clause> ::= (else exp ...)
;; <guard> ::= boolean exp | ()
;; <pattern> :: =
;;        ,var  -- matches always and binds the var
;;                 pattern must be linear! No check is done
;;         ?    -- matches always
;;        exp   -- comparison with exp (using equal?)
;;        (<pattern1> <pattern2> ...) -- matches the list of patterns
;;        (<pattern1> . <pattern2>)  -- ditto
;;        ()    -- matches the empty list

(define-syntax pmatch
  (syntax-rules (else guard)
    [(_ v (e ...) ...)
     (pmatch-aux #f v (e ...) ...)]
    [(_ v name (e ...) ...)
     (pmatch-aux name v (e ...) ...)]))

(define-syntax pmatch-aux
  (syntax-rules (else guard)
    [(_ name (rator rand ...) cs ...)
     (let ([v (rator rand ...)])
       (pmatch-aux name v cs ...))]
    [(_ name v)
     (begin
       (if 'name
           (printf "pmatch ~s failed\n~s\n" 'name v)
           (printf "pmatch failed\n ~s\n" v))
       (error 'pmatch "match failed"))]
    [(_ name v (else e0 e ...)) (begin e0 e ...)]
    [(_ name v (pat (guard g ...) e0 e ...) cs ...)
     (let ([fk (lambda () (pmatch-aux name v cs ...))])
       (ppat v pat (if (and g ...) (begin e0 e ...) (fk)) (fk)))]
    [(_ name v (pat e0 e ...) cs ...)
     (let ([fk (lambda () (pmatch-aux name v cs ...))])
       (ppat v pat (begin e0 e ...) (fk)))]))

(define-syntax ppat
  (syntax-rules (? comma unquote)
    [(_ v ? kt kf) kt]
    [(_ v () kt kf) (if (null? v) kt kf)]
    [(_ v (unquote var) kt kf)
     (let ((var v)) kt)]
    [(_ v (x . y) kt kf)
     (if (pair? v)
         (let ([vx (car v)] [vy (cdr v)])
	   (ppat vx x (ppat vy y kt kf) kf))
         kf)]
    [(_ v lit kt kf) (if (equal? v (quote lit)) kt kf)]))

;; Testing
(define test-failed #f)

(define-syntax test
  (syntax-rules ()
    [(_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ([expected expected-result]
              [produced tested-expression])
         (or (equal? expected produced)
             (begin
               (set! test-failed #t)
               (printf "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                       'tested-expression expected produced)))))]))

;; My stuffs
(define pp
  (lambda things
    (pretty-print things)))

(define repeat-func
  (lambda (i f)
    (unless (= i 0) (f) (repeat-func (- i 1) f))))

(define-syntax repeat
  (syntax-rules ()
    [(_ i e)
     (repeat-func i (lambda () e))]))

(define-syntax reflect
  ;; Goal for debugging
  (syntax-rules ()
    [(_ msg x)
     (project (x) (begin (pp msg) (pp x) succeed))]))

(define dedup
  (lambda (ls)
    (cond
     [(null? ls) '()]
     [(let ([a (car ls)]
            [d (cdr ls)])
        (cond
         [(memq a d) (dedup d)]
         [else `(,a . ,(dedup d))]))])))

(define str->sy string->symbol)
(define sy->str symbol->string)
(define str-app string-append)

(define list-ref
  (lambda (i ls)
    (if (= i 0) (car ls)
        (list-ref (- i 1) (cdr ls)))))



;;; Relations
(define reflect
  (lambda (x)
    (project (x)
      (begin (display x) (newline)
             succeed))))

(define select
  (lambda (x ls res)
    (fresh (a d)
      (== ls (cons a d))
      (conde [(== x a)  (== res d)]
             [(fresh (resd)
                (== res (cons a resd))
                (select x d resd))]))))

(define select-many
  (lambda (l1 l l2)
    (conde [(nullo l1)  (== l l2)]
           [(fresh (x l1d ld)
              (cro l1 x l1d)
              (cro l  x ld)
              (select-many l1d ld l2))]
           [(fresh (x y l1d l2d ld)
              (cro l1 x l1d)
              (cro l2 y l2d)
              (cro l y ld)
              (select-many (cons x l1d) ld l2d))])))

(define appendo
  (lambda (l1 l2 l)
    (conde
     [(== '() l1) (== l l2)]
     [(fresh (l1a l1d ld)
        (== `(,l1a . ,l1d)l1)
        (== `(,l1a . ,ld) l)
        (appendo l1d l2 ld))])))

(define append*o
  (lambda (l* l)
    (conde
     [(== '() l*) (== '() l)]
     [(fresh (x)
        (== l* `(,x))
        (== l x))]
     [(fresh (l*a l*b l*d l*bd)
        (== `(,l*a ,l*b . ,l*d) l*)
        (appendo l*a l*bd l)
        (append*o `(,l*b . ,l*d) l*bd))])))


(define list-splito-core
  (lambda (l1^ l2^ n^ l1 l2)
    (conde
     [(== n^ (build-num 0))
      (reverseo l1^ l1)
      (== l2^ l2)]

     [(fresh (l2^car l2^cdr n)
        (;; This must be first, otherwise predo
         ;; can keep running forever
         cro l2^ l2^car l2^cdr)
        (predo n^ n)
        (list-splito-core `[,l2^car . ,l1^]
                          l2^cdr n l1 l2))])))

(define list-splito
  (lambda (l n l1 l2)
    (list-splito-core '[] l n l1 l2)))

(define list-heado
  (lambda (l n l1)
    (fresh (l2)
      (list-splito l n l1 l2))))

(define list-tailo
  (lambda (l n l2)
    (fresh (l1)
      (list-splito l n l1 l2))))

(define list-refo
  (lambda (l n x)
    (fresh (l1)
      (list-tailo l n l1)
      (caro l1 x))))

(define lengtho
  (lambda (ls len)
    (conde [(nullo ls)  (zeroo len)]
           [(fresh (x xs n)
              (pluso n (build-num 1) len)
              (cro ls x xs)
              (lengtho xs n))])))

(define membero
  (lambda (mem ls)
    (fresh (a d)
      (== ls `(,a . ,d))
      (conde [(== a mem)]
             [(=/= a mem) (membero mem d)]))))

(define reverseo
  (lambda (x y)
    (let reverseo ([x x] [o '()] [y y])
      (conde
       [(== '() x) (== y o)]
       [(fresh (a d)
          (== `(,a . ,d) x)
          (reverseo-core d `(,a . ,o) y))]))))

(define half-halfo
  (lambda (l l1 l2)
    (conde [(nullo l)  (nullo l1) (nullo l2)]
           [(fresh (la ld l1d)
              (cro l la ld)
              (cro l1 la l1d)
              (half-halfo ld l2 l1d))])))

(define rembero
  ;; If x is in l, remove the first occurence of x in l -> res
  ;; If x is not in l, res is #f
  (lambda (x l res)
    (conde
     [(== l '()) (== res #f)]
     [(== l `(,x . ,res))]
     [(fresh (la ld recur)
        (== l `(,la . ,ld))
        (=/= x la)
        (rembero x ld recur)
        (conde
         [(== recur #f) (== res #f)]
         [(=/= recur #f) (== res `(,la . ,recur))]))])))


;; Higher order stuff

(define composeo
  (lambda (f1 f2)
    (lambda (x z)
      (fresh (y) (f1 x y) (f2 y z)))))

(define-syntax naf
  (syntax-rules ()
    [(_ goal)
     (conda [goal fail]
            [succeed])]))

(define same-length
  (lambda (l1 l2)
    (conde
     [(nullo l1) (nullo l2)]
     [(fresh (a1 d1 a2 d2)
        (== l1 (cons a1 d1))
        (== l2 (cons a2 d2))
        (same-length d1 d2))])))

(define mapo
  (lambda (f l out)
    (conde
     [(== l '()) (== out '())]
     [(fresh (a d fa d-out)
        (== l `(,a . ,d))
        (f a fa)
        (== out `(,fa . ,d-out))
        (mapo f d d-out))])))

(define mapo2
  (lambda (f l1 l2 out)
    (conde [(== l1 '()) (== out '())]
           [(fresh (a1 d1 a2 d2 fa d-out)
              (== l1 `(,a1 . ,d1))
              (== l2 `(,a2 . ,d2))
              (f a1 a2 fa)
              (== out `(,fa . ,d-out))
              (mapo2 f d1 d2 d-out))])))

;; reif.ss
(define truet (lambda () (lambda (?) (== #t ?))))
(define falset (lambda () (lambda (?) (== #f ?))))

(define ==t
  (lambda (x y)
    (lambda (?)
      (conde
       [(== #t ?) (== x y)]
       [(== #f ?) (=/= x y)]))))

(define =/=t
  (lambda (x y) (negt (==t x y))))

(define negt
  (lambda (g)
    (lambda (?)
      (conde
       [(== #t ?) (g #f)]
       [(== #f ?) (g #t)]))))

(define-syntax conjt
  ;; A conjunction test
  (syntax-rules ()
    [(_) (truet)]
    [(_ g) g]
    [(_ g1 g2 gs ...)
     (lambda (?)
       (conde
        [(g1 #t) ((conjt g2 gs ...) ?)]
        [(== #f ?) (g1 #f)]))]))

(define-syntax disjt
  ;; A disjunction test
  (syntax-rules ()
    [(_) (falset)]
    [(_ g) g]
    [(_ g1 g2 gs ...)
     (lambda (?)
       (conde
        [(== #t ?) (g1 #t)]
        [(g1 #f)  ((disjt g2 gs ...) ?)]))]))

(define-syntax fresht
  (syntax-rules ()
    [(_ (idens ...) gs ...)
     (lambda (?) (fresh (idens ...) ((conjt gs ...) ?)))]))

(define-syntax condo
  ;; Literally the relational version of 'cond'
  ;; Fails if no clauses match
  (syntax-rules (else)
    [(_ [else]) succeed]
    [(_ [else g]) g]
    [(_ [else g1 g2 g* ...])
     (fresh () g1 g2 g* ...)]
    [(_ [test g* ...] c* ...)
     (conde
      [(test #t) g* ...]
      [(test #f) (condo c* ...)])]
    [(_) fail]))
