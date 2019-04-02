(load "coq.ss")

(pp (inj '+))
#!eof

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
       (-> S-eq ih P)
       ;; assumption: ih, P; goal: ig
       (fresh (plus-Sr)
         (-> (copy =sym) plus-S plus-Sr)
         (-> rwl plus-Sr P^))
       ;; assumption: ih, P, P^; goal: ig
       (-> P^ P P^^)
       ;; assumption: ih, P, P^, P^^; goal: ig
       (-> (copy =sym) P^^ ig)))))
