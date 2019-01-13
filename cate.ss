(define user-eqs
  '[])

(define assoc-ops '(c * +))
(define commu-ops '(* +))

(define ->
  (lambda (x y name)
    (fresh (a b c op)
      (conde
       [;; Associativity
        (== x `(,op ,a (,op ,b ,c)))
        (== y `(,op (,op ,a ,b) ,c))
        (membero op assoc-ops)
        (== name 'assoc)]
       [;; Commutativity
        (== x `(,op ,a ,b))
        (== y `(,op ,b ,a))
        (membero op commu-ops)
        (== name 'commu)]
       [(== x `(c ,a 1))   (== y a)         (== name 'idr)]
       [(== x `(c 1 ,a))   (=/= a 1) (== y a) (== name 'idl)]
       [(== x `(,op ,a ,b))
        (== y `(,op ,c ,b))
        (fresh (n)
          (== name (cons '< n))
          (-> a c n))]
       [(== x `(,op ,a ,b))
        (== y `(,op ,a ,c))
        (fresh (n)
          (== name (cons '> n))
          (-> b c n))]

       ;; Problem-specific
       [(== x `(not (+ (not (+ ,a ,b))
                    (not (+ ,a (not ,b))))))
        (== y a)
        (== name 'robbins)
        ]
       [(membero (list x y name) user-eqs)]
       ))))


(define <->
  (lambda (x y name)
    (conde [(-> x y name)]
           [(fresh (n)
              (== name (cons 'rev n))
              (-> y x n))])))

(arcs
 (lambda (tail)
   (>> (run-nore #f (weight name head)
                 (== weight 1)
                 (<-> tail head name))
       remove-duplicates)))

(queue-insert greedy-insert)

(define size
  (lambda (x)
    (cond [(var? x)   1]
          [(pair? x)  (+ (size (car x))
                         (size (cdr x)))]
          [(null? x)  0]
          [else       1])))

(goal
 (lambda (s)
   (project (s)
     (if (and (<= (size s) 1)
            (not (occurs 1 s)))
         succeed
         fail))))

(heuristic   size)
(start-state 'x)
