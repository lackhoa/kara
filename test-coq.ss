(pp '(-> a (-> b a)))
(pp
 (run* (q) (!- '() '(lambda x (lambda y x)) q)))

(pp '())
(pp
 (run* (q) (!- '() '(lambda x (x x)) q)))

(pp "Expressions typed int -> int")
(pp
 (run 5 (q) (!- '() q '(-> int int))))

(pp "Fix-point factorial")
(pp
 (run 5 (q) (!- '() '(fix (lambda !
                           (lambda n
                             (if (zero? n) 1 (* (! (sub1 n)) n)))))
               q)))

(pp "The final test")
(pp (run 14 (e t) (!- '() e t)))
