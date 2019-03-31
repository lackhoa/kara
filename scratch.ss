(replace-string "f" "f")

(define lookup
  (lambda (x env)
    (let ([a (car env)])
      (cond
       [(eq? x (lhs a)) (rhs a)]
       [else (lookup x (cdr env))]))))

(define lookupo
  (lambda (x env t)
    (fresh (y b)
      (== `((,y ,b) . ,rest) env)
      (conde
       [(== y x) (== b t)]
       [(=/= y x) (lookupo x rest t)]))))

(define lookup
  (lambda (x env)
    (cond
     [(null? env) #f]
     [else
      (let ([a (car env)])
        (cond
         [(eq? x (lhs a)) a]
         [else (lookup x (cdr env))]))])))

(define lookupo
  (lambda (x env t bound?)
    (conde
     [(== '() env) (== #f bound?)]
     [(fresh (y b)
        (== `((,y ,b) . ,rest) env)
        (conde
         [(== y x) (== #t bound?) (== b t)]
         [(=/= y x) (lookupo x rest t bound?)]))])))

(define case1
  (lambda (x env)
    (cond
     [(lookup x env) => rhs]
     [else 'Fail])))

(define case1o
  (lambda (x env out)
    (conde
     [(lookup x env out #t)]
     [(lookup x env 'unbound #f)
      (== 'Fail out)])))

(define case2
  (lambda (x y env)
    (cond
     [(lookup x env) => rhs]
     [(lookup y env) => rhs]
     [else 'Fail])))

(define case2o
  (lambda (x y env out)
    (conde
     [(lookup x env out #t)]
     [(lookup x env '? #f) (lookup y env out #t)]
     [(lookup x env '? #f) (lookup y env '? #f)
      (== 'Fail out)])))

;; From relation
(define conso (lambda (a d ls) (== `(,a . ,d) ls)))
;; To pseudo-function 1
(define cart (lambda (ls) (lambda (a) (fresh (d) (conso a d ls)))))
;; To pseudo-function 2
(define cdrt (lambda (ls) (lambda (d) (fresh (a) (conso a d ls)))))

(define membert
  (lambda (x ls)
    (lambda (?)
      (conde
       [(== '() ls) (== #f ?)]
       [(fresh (a d)
          (== `(,a . ,d) ls)
          (condo
           [(==t a x) (== #t ?)]
           [else ((membert x d) ?)]))]))))

(define subo
  (lambda (x v f g)
    (fresh ()
      (symbolo x)
      ((membert x func-syms) #f)
      (let subo ([f f] [g g])
        (conde
         [(== x f) (== v g)]
         [(=/= x f)
          (conde
           [(conde [(== `() f)] [(numbero f)] [(symbolo f)])
            (== f g)]
           [(fresh (fa fd ga gd)
              (== `(,fa . ,fd) f)
              (== `(,ga . ,gd) g)
              (subo fa ga)
              (subo fd gd))])])))))
