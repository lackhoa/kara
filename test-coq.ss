(pp '(-> a (-> b a)))
(pp
 (run* (q) (typeo init-ctx '(lambda x (lambda y x)) q)))

(pp "Fails")
(pp
 (run* (q) (typeo init-ctx '(lambda x (x x)) q)))

(pp "Expressions typed int -> int")
(pp
 (run 5 (q) (typeo init-ctx q '(-> int int))))
