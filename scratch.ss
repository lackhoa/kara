([(#(t1 q) #(t2 q) #(S+ q) #(S+ q))
  ()
  ((walko #(t2 q) #(S+ q) #(0 r))
   (walko #(t1 q) #(S+ q) #(0 r)))]

 [(#(t1 q) #(t2 q) #(S q) #(S+ q))
  (((#(0 r) #(1 r))))
  ((ext-S-checko #(0 r) #(1 r) #(S q) #(S+ q))
   (vector?o #(0 r) #t)
   (walko #(t2 q) #(S q) #(1 r))
   (walko #(t1 q) #(S q) #(0 r)))]

 [(#(t1 q) #(t2 q) #(S q) #(S+ q))
  (((#(1 r) #(0 r))))
  ((ext-S-checko #(0 r) #(1 r) #(S q) #(S+ q))
   (vector?o #(0 r) #t)
   (vector?o #(1 r) #f)
   (walko #(t2 q) #(S q) #(0 r))
   (walko #(t1 q) #(S q) #(1 r)))]

 [(#(t1 q) #(t2 q) #(S q) #(S+ q))
  ((((#(3 r) . #(0 r)) (#(4 r) . #(1 r))))
   ((#(2 r) #f)))
  ((unifyo #(0 r) #(1 r) #(2 r) #(S+ q))
   (unifyo #(3 r) #(4 r) #(S q) #(2 r))
   (pair?o (#(4 r) . #(1 r)) #t)
   (pair?o (#(3 r) . #(0 r)) #t)
   (vector?o (#(4 r) . #(1 r)) #f)
   (vector?o (#(3 r) . #(0 r)) #f)
   (walko #(t2 q) #(S q) (#(4 r) . #(1 r)))
   (walko #(t1 q) #(S q) (#(3 r) . #(0 r))))]

 [(#(t1 q) #(t2 q) #(S q) #f)
  (((#(1 r) #(0 r))))
  ((pair?o #(0 r) #f)
   (pair?o #(1 r) #t)
   (vector?o #(0 r) #f)
   (vector?o #(1 r) #f)
   (walko #(t2 q) #(S q) #(0 r))
   (walko #(t1 q) #(S q) #(1 r)))]

 [(#(t1 q) #(t2 q) #(S q) #f)
  (((#(0 r) #(1 r))))
  ((pair?o #(0 r) #f)
   (vector?o #(1 r) #f)
   (vector?o #(0 r) #f)
   (walko #(t2 q) #(S q) #(1 r))
   (walko #(t1 q) #(S q) #(0 r)))])


(()
 ([((#(S q) #(S+ q)))
   ()
   ((walko #(t2 q) #(S+ q) #(0 r))
    (walko #(t1 q) #(S+ q) #(0 r)))]
  [()
   (((#(0 r) #(1 r))))
   ((ext-S-checko #(0 r) #(1 r) #(S q) #(S+ q))
    (vector?o #(0 r) #t)
    (walko #(t2 q) #(S q) #(1 r))
    (walko #(t1 q) #(S q) #(0 r)))]
  [()
   (((#(1 r) #(0 r))))
   ((ext-S-checko #(0 r) #(1 r) #(S q) #(S+ q))
    (vector?o #(0 r) #t)
    (vector?o #(1 r) #f)
    (walko #(t2 q) #(S q) #(0 r))
    (walko #(t1 q) #(S q) #(1 r)))]
  [()
   ((((#(3 r) . #(0 r)) (#(4 r) . #(1 r)))) ((#(2 r) #f)))
   ((unifyo #(0 r) #(1 r) #(2 r) #(S+ q)) (unifyo #(3 r) #(4 r) #(S q) #(2 r))
    (pair?o (#(4 r) . #(1 r)) #t) (pair?o (#(3 r) . #(0 r)) #t)
    (vector?o (#(4 r) . #(1 r)) #f)
    (vector?o (#(3 r) . #(0 r)) #f)
    (walko #(t2 q) #(S q) (#(4 r) . #(1 r)))
    (walko #(t1 q) #(S q) (#(3 r) . #(0 r))))]
  [((#(S+ q) #f))
   (((#(1 r) #(0 r))))
   ((pair?o #(0 r) #f) (pair?o #(1 r) #t) (vector?o #(0 r) #f)
    (vector?o #(1 r) #f) (walko #(t2 q) #(S q) #(0 r))
    (walko #(t1 q) #(S q) #(1 r)))]
  [((#(S+ q) #f))
   (((#(0 r) #(1 r))))
   ((pair?o #(0 r) #f)
    (vector?o #(1 r) #f)
    (vector?o #(0 r) #f)
    (walko #(t2 q) #(S q) #(1 r))
    (walko #(t1 q) #(S q) #(0 r)))]))

(unifier
 #<procedure unify at ak.ss:1415>
 (((foo (nom-tag "a") (nom-tag "a")) foo (nom-tag "b") (nom-tag "b"))))
