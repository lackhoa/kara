#lang racket
(require "lang/kara.rkt")

(def (head arrow)
  (lcar arrow))

(def (tail arrow)
  (lcadr arrow))

; Returns nodes that can be directly reached from `v`
(def (direct-reach v E)
  (map tail (filter (lam (arrow)
                      (equal? (head arrow) v))
                    E)))

; Get reachable vertices from the vertice `v`
; in the (including itself)
(def (reach v E)
  (let reach-core ([already (set v)]
                   [new     (set v)])
    (if (null? new)
        already
      (let ([newer null])
        (for-each
          (lam (new-iter)
            (set-union! newer
                        (set-difference (direct-reach new-iter E)
                                        already)))
          new)
        (reach-core (set-union new already) newer)))))

; Get all the vertices reachable from the set sub-V
(def (span sub-V E)
  (lreduce (lam (first rest)
             (set-union (reach first E) rest))
           null
           sub-V))

(def (min-starting-set V E)
  (let ([S null])
    (for-each
      (lam (sub-V)
        (unless (exists (lam (S-iter)
                          (is-subset S-iter sub-V))
                        S)
          (when (equal? V (span sub-V E))
            (set-union! S sub-V))))
      (powerset (set->list V)))
    S))
