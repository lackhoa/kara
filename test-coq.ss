(pp '(-> a (-> b a)))
(pp
 (run* (q) (typeo init-ctx '(lambda x (lambda y x)) q)))

(pp "Fails")
(pp
 (run* (q) (typeo init-ctx '(lambda x (x x)) q)))

(pp "Expressions typed int -> int")
(pp
 (run 5 (q) (typeo init-ctx q '(-> int int))))

(pp
 (run 1 (q)
   (fresh (base step)
     (indo base step plus0r)
     ;; goal: base; step
     (fresh (X) (subo X 0 plus0 base))
     ;; goal: step
     (fresh (ih ig P P^ P^^)
       (== `(-> ,ih ,ig) step)
       ;; assumption: ih; goal: ig
       (mpo S-eq ih P)
       ;; assumption: ih, P; goal: ig
       (fresh (plus-Sr)
         (mpo (copy =sym) plus-S plus-Sr)
         (mpo eql plus-Sr P^))
       ;; assumption: ih, P, P^; goal: ig
       (mpo P^ P P^^)
       ;; assumption: ih, P, P^, P^^; goal: ig
       (mpo (copy =sym) P^^ ig)))))
