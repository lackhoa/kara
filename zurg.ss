(define toy-list
  '[1 3 3 4 5])

;; A state transition begins with selecting
;; one or two person from Here, then move to There
;; ("Here" is the side with the flashlight)
(arcs
 (lambda (tail)
   (run* (weight name head)
     (pmatch tail
       [(,here ,h^ ,there ,t^)
        (fresh (chosen h)
          ;; Selct toys for the bridge
          (conde [(fresh (_0 _1) (== chosen (list _0 _1)))]
                 [(fresh (_0)    (== chosen (list _0)))])
          (select-many chosen h^ h)
          (== name  chosen)
          ;; Time concern
          (project (chosen)
            (== weight  (apply max chosen)))
          ;; Unload toys
          (fresh (t)
            (appendo chosen t^ t)
            (project (t h)
              (== head (list there (sort < t)
                            here  (sort < h))))))]))))

(heuristic
 (lambda (state)
   (pmatch state
     [(left ,? right ,r)  (- (length r) (length toy-list))]
     [(right ,r left ,?)  (- (length r) (length toy-list))])))

(goal
 (lambda (state)
   (fresh (_0) (== state `(right ,_0 left [])))))

(start-state
 `(left ,toy-list right []))
