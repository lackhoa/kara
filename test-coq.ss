(load "coq.ss")

(pp
 (run 1 (q)
   (fresh (base step
                ih ig
                P P^ P^^)
     (indo base step plus0r)
     ;; goal: base; step
     (fresh (X) (subo X 0 plus0 base))
     ;; goal: step
     (== `(-> ,ih ,ig) step)
     ;; assumption: ih; goal: ig
     (-> (inj 'S) ih P)
     ;; assumption: ih, P; goal: ig
     (fresh (plus-Sr)
       (-> (copy =sym) plus-S plus-Sr)
       (-> rwl plus-Sr P^))
     ;; assumption: ih, P, P^; goal: ig
     (-> P^ P P^^)
     ;; assumption: ih, P, P^, P^^; goal: ig
     (-> (copy =sym) P^^ ig)
     (== q ig))))
