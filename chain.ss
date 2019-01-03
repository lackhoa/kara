(define arcs
  ;; A state is a bunch of lists representing chains
  (lambda (tail)
    (>> (run* (weight name head)
          (== weight 1)
          (conde [(== name 'break)
                  (fresh (c cs c1 c2)
                    (select c tail cs)
                    (appendo* (list c1 c2 '[0]) c)
                    (fresh (new)
                      (rembero '[] (list '[-] c1 c2) new)
                      (appendo new cs head)))]))
        (l> map
            (l> apply (lambda (w n head)
                        (list w n (sort
                                   (lambda (x y)
                                     (or (equal? x '[-])
                                        (< (length x) (length y))))
                                   head)))))
        remove-duplicates)))


(define heuristic length)

(define goal
  (lambda (x) (reverseo x x)))

(define main
  (lambda (solution)
    (search '[[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]] solution)))
