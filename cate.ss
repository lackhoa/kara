(define ->
  (lambda (x y name)
    (fresh (a b c)
      (conde
       ;; Category theorey
       [(== x `(* ,a (* ,b ,c)))
        (== y `(* (* ,a ,b) ,c))
        (== name 'assoc)]
       [(== x `(* ,a 1))   (== y a)         (== name 'idr)]
       [(== x `(* 1 ,a))   (=/= a 1) (== y a) (== name 'idl)]
       [(== x `(* ,a ,b))
        (== y `(* ,c ,b))
        (fresh (n)
          (== name (cons '< n))
          (-> a c n))]
       [(== x `(* ,a ,b))
        (== y `(* ,a ,c))
        (fresh (n)
          (== name (cons '> n))
          (-> b c n))]

       ;; Problem-specific
       ;; [(== x '(* f a)) (== y '(* b f))             (== name 'f)]
       ;; [(== x 'y)       (== y '(* f x))             (== name 'y)]
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
     (if (and (<= (size s) 2)
            (not (occurs 1 s)))
         succeed
         fail))))

(heuristic   size)
(start-state '(* f x1))
