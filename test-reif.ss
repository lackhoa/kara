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
