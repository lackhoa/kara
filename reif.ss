(define ifo
  (lambda (test then else)
    (fresh (t)
      (test t)
      (conde [(== t #t) then]
             [(== t #f) else]))))

(define ==t
  (lambda (x y)
    (lambda (t)
      (conde [(== x y) (== t #t)]
             [(=/= x y) (== t #f)]))))

(define-syntax fresht
  (syntax-rules ()
    [(_ (idens ...) g)
     (lambda (t)
       (fresh (idens ...)
         (g t)))]
    [(_ (idens ...) g1 g2 gs ...)
     (lambda (t)
       (fresh (idens ...)
         (ifo g1 ((fresht () g2 gs ...) t)
              (== t #f))))]))

(define-syntax condt
  (syntax-rules ()
    [(_ [g gs ...])
     (fresht () g gs ...)]
    [(_ [g1 g1s ...] [g2 g2s ...] ...)
     (lambda (t)
       (ifo (fresht () g1 g1s ...) (== t #t)
            ((condt [g2 g2s ...] ...) t)))]))



;;; Test functions!
(define tfilter
  (lambda (ct2 ees fs)
    (conde [(== ees '()) (== fs '())]
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
      (ifo (==t x e) succeed
           (memberd x es)))))

(define memberdt
  (lambda (x es)
    (lambda (t)
      (conde [(== t #t) (memberd x es)]
             [(== t #f) (fresh (_0)
                         (mapo (lambda (in _out) (fresh () (== in _out) (=/= in x)))
                               es
                               _0))]))))

(define first-duplicate
  (lambda (x ees)
    (fresh (e es)
      (== ees `(,e . ,es))
      (ifo (memberdt e es) (== x e)
           (first-duplicate x es)))))

(define tree-memberdt
  (lambda (e tree)
    (lambda (t)
      (conde [(== tree '()) (== t #f)]
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
