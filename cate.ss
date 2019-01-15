(define assoc-ops '(c * +))
(define commu-ops '(* +))

(define size
  (lambda (x)
    (cond [(var? x)   1]
          [(pair? x)  (+ (size (car x))
                         (size (cdr x)))]
          [(null? x)  0]
          [else       1])))

(define ->
  (lambda (t1 t2)
    (conde [(fresh (l r x y ny )
              (== t1 `(not (+ ,l ,r)))
              (fresh (y/ny)
                (conde [(== y y/ny)       (== ny `(not y/ny))]
                       [(== y `(not y/ny))  (== ny y/ny)]))
              (== l `(not (+ ,x ,y)))
              (== r `(not (+ ,x ,ny))))])))

(define path
  (lambda (start p end)
    (conde [(== start end)  (nullo p)]
           [(=/= start end)  (fresh (pa pd)
                             (cro p pa pd)
                             (-> start pa)
                             (path pa pd end))])))
