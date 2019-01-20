(define assoc-ops '(c * +))
(define commu-ops '(* +))

(define size
  (lambda (x)
    (cond [(var? x)   1]
          [(pair? x)  (+ (size (car x))
                         (size (cdr x)))]
          [(null? x)  0]
          [else       1])))

(define rev
  (lambda (fun) (lambda (x y) (fun y x))))

(define robbins
  ;; Encode rules: right now let's just use one
  ;; T is the encompassing formula within which t transform to t+
  ;; T+ is just T after the transformation
  (lambda (t t+)
    (fresh (l r y)
      (ac t `(not (+ ,l ,r)))
      (ac l `(not (+ ,t+ ,y)))
      (ac r `(not (+ ,t+ (not ,y)))))))

(define hun
  (lambda (t t+)
    (fresh (l r y)
      (ac t `(+ ,l ,r))
      (ac l `(not (+ (not ,t+) (not ,y))))
      (ac r `(not (+ (not ,t+) ,y))))))

(define ac*
  (lambda (l1 l2)
    (conde [(nullo l1) (nullo l2)]
           [(fresh (a1 d1 a2 d2)
              (== l1 (cons a1 d1))
              (== l2 (cons a2 d2))
              (ac a1 a2)
              (ac* d1 d2))])))

(define ac
  (lambda (t1 t2)
    (conde [(== t1 t2)]
           [(fresh (disj1 disj2)
              (== t1 `(+ . ,disj1))
              (== t2 `(+ . ,disj2))
              (pairo disj1) (pairo disj2)
              (fresh (disj3)
                (permute disj1 disj3)
                (ac* disj2 disj3)))]
           [(fresh (body1 body2)
              (== t1 `(not ,body1))
              (== t2 `(not ,body2))
              (ac body1 body2))])))
