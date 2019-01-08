(define user-eqs
  '[])

(define assoc-ops '(c * +))
(define commu-ops '(* +))

(define ->
  (lambda (x y name)
    (fresh (a b c op)
      (conde
       ;; [;; Associativity
       ;;  (== x `(,op ,a (,op ,b ,c)))
       ;;  (== y `(,op (,op ,a ,b) ,c))
       ;;  (membero op assoc-ops)
       ;;  (== name 'assoc)]
       ;; [;; Commutativity
       ;;  (== x `(,op ,a ,b))
       ;;  (== y `(,op ,b ,a))
       ;;  (membero op commu-ops)
       ;;  (== name 'commu)]
       ;; [(== x `(c ,a 1))   (== y a)         (== name 'idr)]
       ;; [(== x `(c 1 ,a))   (=/= a 1) (== y a) (== name 'idl)]
       ;; [(== x `(,op ,a ,b))
       ;;  (== y `(,op ,c ,b))
       ;;  (fresh (n)
       ;;    (== name (cons '< n))
       ;;    (-> a c n))]
       ;; [(== x `(,op ,a ,b))
       ;;  (== y `(,op ,a ,c))
       ;;  (fresh (n)
       ;;    (== name (cons '> n))
       ;;    (-> b c n))]


       ;; Problem-specific
       [(== x `(not (+ ,a ,b)))
        (== y a)
        (== name 'winker2)
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
   (>> (run* (weight name head)
         (== weight 1)
         (<-> tail head name))
       remove-duplicates)))

(queue-insert greedy-insert)

(goal
 (lambda (s)
   (project (s)
     (if (and (<= (size s) 5)
            (not (occurs 1 s)))
         succeed
         fail))))

(heuristic   size)
(start-state '(not (+ j k)))
;; (not (+ (not (+ j k))
;;       (not (+ j (not k)))))
