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
