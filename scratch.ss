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

(define copy
  (lambda (t)
    (let-values
        ([(res _met)
          (let copy ([t t] [met '()])
            (cond
             [(var? t)
              (cond
               [(assq t met)
                => (lambda (asso) (values (my-rhs asso) met))]
               [else
                (let ([v (var (vector-ref t 0))])
                  (values v `((,t ,v) . ,met)))])]

             [(pair? t)
              (let-values ([(a met+) (copy (car t) met)])
                (let-values ([(d met++) (copy (cdr t) met+)])
                  (values `(,a . ,d) met++)))]

             [else (values t met)]))])
      res)))



(((#(t1 0) #(t2 0) #f)
  ()
  ((eq?o #(t1+ 1) #(t2+ 1) #t)
   (walko #(t2 0) #f #(t2+ 1))
   (walko #(t1 0) #f #(t1+ 1))))

 ((#(t1 0) #(t2 0) #(S 0))
  ()
  ((ext-S-checko #(t1+ 1) #(t2+ 1) #(S 0) #f)
   (vector?o #(t1+ 1) #t)
   (eq?o #(t1+ 1) #(t2+ 1) #f)
   (walko #(t2 0) #(S 0) #(t2+ 1))
   (walko #(t1 0) #(S 0) #(t1+ 1))))

 ((#(t1 0) #(t2 0) #(S 0))
  ()
  ((ext-S-checko #(t2+ 1) #(t1+ 1) #(S 0) #f) (vector?o #(t2+ 1) #t) (vector?o #(t1+ 1) #f)
   (eq?o #(t1+ 1) #(t2+ 1) #f) (walko #(t2 0) #(S 0) #(t2+ 1))
   (walko #(t1 0) #(S 0) #(t1+ 1))))

 ((#(t1 0) #(t2 0) #(S 0))
  ()
  ((unifyo #(a1 2) #(a2 2) #(S 0) #f) (pair?o (#(a2 2) . #(d2 2)) #t)
   (pair?o (#(a1 2) . #(d1 2)) #t)
   (vector?o (#(a2 2) . #(d2 2)) #f)
   (vector?o (#(a1 2) . #(d1 2)) #f)
   (eq?o (#(a1 2) . #(d1 2)) (#(a2 2) . #(d2 2)) #f)
   (walko #(t2 0) #(S 0) (#(a2 2) . #(d2 2)))
   (walko #(t1 0) #(S 0) (#(a1 2) . #(d1 2)))))

 ((#(t1 0) #(t2 0) #(S 0))
  (((#(S^ 2) #f)))
  ((unifyo #(d1 2) #(d2 2) #(S^ 2) #f) (unifyo #(a1 2) #(a2 2) #(S 0) #(S^ 2))
   (pair?o (#(a2 2) . #(d2 2)) #t)
   (pair?o (#(a1 2) . #(d1 2)) #t)
   (vector?o (#(a2 2) . #(d2 2)) #f)
   (vector?o (#(a1 2) . #(d1 2)) #f)
   (eq?o (#(a1 2) . #(d1 2)) (#(a2 2) . #(d2 2)) #f)
   (walko #(t2 0) #(S 0) (#(a2 2) . #(d2 2)))
   (walko #(t1 0) #(S 0) (#(a1 2) . #(d1 2)))))

 ((#(t1 0) #(t2 0) #(S 0))
  ()
  ((pair?o #(t2+ 1) #f) (pair?o #(t1+ 1) #t) (vector?o #(t2+ 1) #f)
   (vector?o #(t1+ 1) #f) (eq?o #(t1+ 1) #(t2+ 1) #f)
   (walko #(t2 0) #(S 0) #(t2+ 1))
   (walko #(t1 0) #(S 0) #(t1+ 1))))

 ((#(t1 0) #(t2 0) #(S 0))
  ()
  ((pair?o #(t1+ 1) #f)
   (vector?o #(t1+ 1) #f)
   (vector?o #(t2+ 1) #f)
   (eq?o #(t1+ 1) #(t2+ 1) #f)
   (walko #(t2 0) #(S 0) #(t2+ 1))
   (walko #(t1 0) #(S 0) #(t1+ 1)))))


(((#(t1 0) #(t2 0) #(S 0))
  (((#(S 0) #f)))
  ((eq?o #(t1+ 2) #(t2+ 2) #t)
   (walko #(t2 0) #(S 0) #(t2+ 2))
   (walko #(t1 0) #(S 0) #(t1+ 2))))

 ((#(t1 0) #(t2 0) #(S 0))
  (((#(S+ 1) #f)))
  ((ext-S-checko #(t1+ 2) #(t2+ 2) #(S 0) #(S+ 1))
   (vector?o #(t1+ 2) #t)
   (eq?o #(t1+ 2) #(t2+ 2) #f)
   (walko #(t2 0) #(S 0) #(t2+ 2))
   (walko #(t1 0) #(S 0) #(t1+ 2))))

 ((#(t1 0) #(t2 0) #(S 0))
  (((#(S+ 1) #f)))
  ((ext-S-checko #(t2+ 2) #(t1+ 2) #(S 0) #(S+ 1))
   (vector?o #(t2+ 2) #t)
   (vector?o #(t1+ 2) #f)
   (eq?o #(t1+ 2) #(t2+ 2) #f)
   (walko #(t2 0) #(S 0) #(t2+ 2))
   (walko #(t1 0) #(S 0) #(t1+ 2))))

 ((#(t1 0) #(t2 0) #(S 0))
  (((#(S+ 1) #f)) ((#(S^ 3) #f)))
  ((unifyo #(d1 3) #(d2 3) #(S^ 3) #(S+ 1))
   (unifyo #(a1 3) #(a2 3) #(S 0) #(S^ 3))
   (pair?o (#(a2 3) . #(d2 3)) #t)
   (pair?o (#(a1 3) . #(d1 3)) #t)
   (vector?o (#(a2 3) . #(d2 3)) #f)
   (vector?o (#(a1 3) . #(d1 3)) #f)
   (eq?o (#(a1 3) . #(d1 3))
         (#(a2 3) . #(d2 3))
         #f)
   (walko #(t2 0) #(S 0) (#(a2 3) . #(d2 3)))
   (walko #(t1 0) #(S 0) (#(a1 3) . #(d1 3))))))

(define E
  (lambda (n)
    (let loop ([k n])
      (if (= k 1) 1
          (+ (* (/ n (+ n (- k) 1))
                (loop (- k 1)))
             1)))))
