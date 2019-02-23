(display
 (let ([x (var 'x)])
   ((== #t x) empty-c)))
(newline)

(display
 (let ([x (var 'x)]
       [y (var 'y)])
   ((conj2 (== #t x) (== y x))
    empty-c)))
(newline)

(display
 (let ([x (var 'x)]
       [y (var 'y)]
       [z (var 'z)])
   ((conj (=/= z x) (== #t x) (== x y))
    empty-c)))
(newline)

(display
 (let ([x (var 'x)]
       [y (var 'y)]
       [z (var 'z)])
   ((conj (=/= z x) (== #t x)(== y x) (== z #t))
    empty-c)))
(newline)

(display
 (let ([x (var 'x)])
   ((disj2 (== x 5) (== x 6)) empty-c)))
(newline)

(display
 (let ([x (var 'x)]
       [y (var 'y)])
   ((disj (== x 5) (== x 6) (=/= x y)) empty-c)))
(newline)

(printf "conde and fresh: x = 6 and y =/= 6\n")
(display
 ((fresh (x y)
    (conde
     [(== x 5) (== x y)]
     [(== x 6)])
    (=/= x y))
  empty-c))
(newline)

(printf "Shadowing\n")
(display
 ((fresh (x)
    (== x 5)
    (fresh (x)
      (== x 6)))
  empty-c))
(newline)

(printf "Run & subsumption\n")
(display
 (run (fresh (x y)
        (=/= x 5)
        (=/= `(,x ,y) '(5 6)))))
(newline)
