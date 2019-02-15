(define ==t
  (lambda (x y)
    (lambda (t)
      (conde
       [(== t #t) (== x y)]
       [(== t #f) (=/= x y)]))))

(define ifo
  (lambda (test then else)
    (fresh (t)
      (test t)
      (conde
       [(== #t t) then]
       [(== #f t) else]))))

(define negt
  (lambda (test)
    (lambda (t)
      (conde
       [(== t #t) (test #f)]
       [(== t #f) (test #t)]))))

(define-syntax conjt
  ;; A conjunction test
  (syntax-rules ()
    [(_ g) g]
    [(_ g1 g2 gs ...)
     (lambda (t)
       (ifo g1 ((conjt g2 gs ...) t)
            (== t #f)))]))

(define-syntax disjt
  ;; A disjunction test
  (syntax-rules ()
    [(_ g) g]
    [(_ g1 g2 gs ...)
     (lambda (t)
       (ifo g1 (== t #t)
            ((disjt g2 gs ...) t)))]))

(define-syntax condo
  ;; Literally the relational version of 'cond'
  ;; Fails if no clauses match
  (syntax-rules (else)
    [(_ [else g]) g]

    [(_ [else g1 g2 g* ...])
     (fresh () g1 g2 g* ...)]

    [(_ [test g] c* ...)
     (ifo test g
          (condo c* ...))]

    [(_ [test g1 g2 g* ...] c* ...)
     (ifo test (fresh () g1 g2 g* ...)
          (condo c* ...))]

    [(_) fail]))


;;; Test functions!
(define tfilter
  (lambda (ct2 ees fs)
    (conde
     [(== ees '()) (== fs '())]
     [(fresh (e es fs+)
        (== ees `(,e . ,es))
        (ifo (ct2 e) (== fs `(,e . ,fs+))
             (== fs fs+))
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
    (lambda (t)
      (conde
       [(== t #t) (memberd x es)]
       [(== t #f) (fresh (_0)
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
    (lambda (t)
      (conde
       [(== tree '()) (== t #f)]
       [(fresh (f l r)
          (== tree `(,f ,l ,r))
          ((condt [(==t e f)]
                  [(tree-memberdt e l)]
                  [(tree-memberdt e r)])
           t))]))))

;; Test cases!
;; (tfilter (lambda (item) (condt [(==t x item)] [(==t y item)]))
;;          '(1 2 3 2 3 3)
;;          fs)
;; (memberd x `(1 ,y 2 3 1))
;; (first-duplicate x `(,a ,b ,c))
;; ((tree-memberdt x t) #t)
;; pure vs impure: epic showdown
(define a-z
  '(a b c d e f g h i j k l m n o p q r s t u v w z y z))
(define-syntax ifa
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((c-inf e))
       (case-inf c-inf
                 (() (ifa b ...))
                 ((f) (inc (loop (f))))
                 ((a) (bind* c-inf g ...))
                 ((a f) (bind* c-inf g ...)))))))
(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (c)
       (inc
        (ifa ((g0 c) g ...)
             ((g1 c) g^ ...) ...))))))
(define-syntax ifu
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((c-inf e))
       (case-inf c-inf
                 (() (ifu b ...))
                 ((f) (inc (loop (f))))
                 ((c) (bind* c-inf g ...))
                 ((c f) (bind* (unit c) g ...)))))))
(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (c)
       (inc
        (ifu ((g0 c) g ...)
             ((g1 c) g^ ...) ...))))))
(define memberd-impure
  (lambda (x ees)
    (fresh (e es)
      (== ees `(,e . ,es))
      (conda
       [(== x e) succeed]
       [succeed (memberd-impure x es)]))))
;; (time (repeat 100000 (lambda () (run* (q) (memberd-impure 'z a-z)))))
;; (time (repeat 100000 (lambda () (run* (q) (memberd 'z a-z)))))
