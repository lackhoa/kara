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
          ((condt
            [(==t e f)]
            [(tree-memberdt e l)]
            [(tree-memberdt e r)])
           ?))]))))


(pp "==t => x is different from y\n")
(pp
 (run* (x y) ((==t x y) #f)))
(newline)

(pp "negt and =/=t => x is the same as y")
(pp (run* (x y) ((=/=t x y) #f)))

(pp "conjt => x is y and y is z\n")
(pp
 (run* (x y z t)
   ((conjt (==t x y) (==t y z)) t)))
(newline)

(pp "disjt => x is y or y is z\n")
(pp
 (run* (x y z t) ((disjt (==t x y) (==t y z)) t)))
(newline)

(pp "condo & memberd => (memberd x x*)\n")
(define memberd
  (lambda (x ees)
    (fresh (e es)
      (== ees `(,e . ,es))
      (condo
       [(==t x e) succeed]
       [else (memberd x es)]))))
(pp
 (run 5 (x x*) (memberd x x*)))
(newline)

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
(time (repeat 100000 (run* (q) (memberd-impure 'z a-z))))
(time (repeat 100000 (run* (q) (memberd 'z a-z))))
