(display "You ran the wrong file!")
#!eof

(fresh (z u)
  (== `(,z ,u var) x)
  (conde
   [(== u y)]
   [(=/= u y)]))

(((#(au0) #(au1) var) #(y)) (#(x) #(y)))
(((;; (;; This is from the qv (doesn't matter)
   ;;  #(x) (#(0) #(y) var))
   ;; (#(y) #(y))
   (#(y) #(au1))
   ;; (#(0) #(au0)) 0 -> au0
   )
  ()
  ())
 ((;; (;; This is from the qv (doesn't matter)
   ;;  #(x) (#(0) #(1) var))
   ;; (#(y) #(y))
   ;; (#(1) #(au1)) 1 -> au1
   ;; (#(0) #(au0)) 0 -> au0
   )
  ((;; (#(1) #(y))
    (#(au1) #(y))
    ))
  ()))
(fresh (au0 au1)
  (== `(,au0 ,au1 var) x)
  (conde [(== y au1)]
         [(=/= y au1)]))

;; Start working on THIS!

(((#(x) (#(z) #(au0) var)))
 (((#(y) #(au0)))
  ()
  ())
 (;; ((#(1) #(au0))) DEL
  (;; ((#(1) #(y))) ->  (#(au0) #(y))
   )
  ()))

(((#(env) ((#(au0) . #(au1)) . #(au2))))
 ((((val . #(t)) #(au1))
   (#(x) #(au0)))
  ()
  ())
 ((((closure #(1) ((#(x) rec . #(1)) . #(au2))) #(au3))
   ((rec . #(1)) #(au1))
   (#(x) #(au0)))
  ()
  ())
 (()
  (((#(x) #(au0))))
  ((lookupt #(x) #(au2) #(t)) #t)))


Note:
- qv: one side is a query variable
- au: one side is an anti-unify variable
- Obvious move: Delete when two sides are eq?
- If one side is neither a query variable or an au variable (and it must be the reverse situation for the other side), it shall be renamed to the other one
