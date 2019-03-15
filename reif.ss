(define truet
  (lambda (?) (== #t ?)))
(define falset
  (lambda (?) (== #f ?)))

(define ==t
  (lambda (x y)
    (lambda (?)
      (conde
       [(== #t ?) (== x y)]
       [(== #f ?) (=/= x y)]))))

(define negt
  (lambda (g)
    (lambda (?)
      (conde
       [(== #t ?) (g #f)]
       [(== #f ?) (g #t)]))))

(define =/=t
  (lambda (x y) (negt (==t x y))))

(define-syntax conjt
  ;; A conjunction test
  (syntax-rules ()
    [(_) truet]
    [(_ g) g]
    [(_ g1 g2 gs ...)
     (lambda (?)
       (conde
        [(g1 #t) ((conjt g2 gs ...) ?)]
        [(== #f ?) (g1 #f)]))]))

(define-syntax disjt
  ;; A disjunction test
  (syntax-rules ()
    [(_) falset]
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

(define-syntax wrapt
  (syntax-rules ()
    [(_ gs ...)
     (lambda (_?) (fresh () (== #t _?) gs ...))]))

;; Next: condt?

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

;;; Test functions!
(define tfilter
  (lambda (ct2 ees fs)
    (conde
     [(== ees '()) (== fs '())]
     [(fresh (e es fs+)
        (== ees `(,e . ,es))
        (condo [(ct2 e) (== fs `(,e . ,fs+))]
               [else (== fs fs+)])
        (tfilter ct2 es fs+))])))

(define duplicate
  (lambda (x xs)
    (fresh (_0 _1 _2)
      (tfilter (lambda (item) (==t x item))
               xs
               `(,_0 ,_1 . ,_2)))))

(define memberd
  (lambda (x ees)
    (fresh (e es)
      (== ees `(,e . ,es))
      (condo
       [(==t x e) succeed]
       [else (memberd x es)]))))

(define memberdt
  (lambda (x es)
    (lambda (?)
      (conde
       [(== ? #t) (memberd x es)]
       [(== ? #f) (fresh (_0)
                   (mapo (lambda (in _out) (fresh () (== in _out) (=/= in x)))
                         es
                         _0))]))))

(define first-duplicate
  (lambda (x ees)
    (fresh (e es)
      (== ees `(,e . ,es))
      (condo
       [(memberdt e es) (== x e)]
       [else (first-duplicate x es)]))))

(define tree-memberdt
  (lambda (e tree)
    (lambda (?)
      (conde
       [(== tree '()) (== ? #f)]
       [(fresh (f l r)
          (== tree `(,f ,l ,r))
          ((condt [(==t e f)]
                  [(tree-memberdt e l)]
                  [(tree-memberdt e r)])
           ?))]))))
