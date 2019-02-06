(define fish1
  (map (lambda (c) (map build-num c))
       '([0 0 1 0] [0 1 1 1] [1 0 1 1] [1 0 2 0] [1 1 1 2] [1 1 2 1] [2 0 2 1] [2 1 2 2])))

(define fish2
  (map (lambda (c) (map build-num c))
       '([0 0 0 1] [0 1 0 2] [0 1 1 1] [0 2 1 2] [1 0 1 1] [1 1 1 2] [1 1 2 1] [1 2 2 2])))

(define nat
  (lambda (x)
    (conde [(== x (build-num 0))]
           [(fresh (x--)
              (minuso x (build-num 1) x--)
              (nat x--))])))

(define integer
  (lambda (x)
    (fresh (n)
      (nat n)
      (conde [(== x n)]
             [(=/= n (build-num 0))
              (== x `(- . ,n))]))))

(define zposo
  (lambda (z)
    (fresh (a d)
      (== z `(,a . ,d))
      (=/= a '-))))

(define zzeroo
  zeroo)

(define znego
  (lambda (z)
    (fresh (_1 _2)
      (== z `(- ,_1 . ,_2)))))

(define znonnego
  (lambda (z)
    (conde [(zeroo z)] [(zposo z)])))

(define abso
  (lambda (x y)
    (conde [(znonnego x) (== y x)]
           [(znego x)
            (== x `(- . ,y))])))

(define zpluso
  (lambda (x y z)
    (conde [(znonnego x) (znonnego y)
            (pluso x y z)]
           [(znego x) (znego y)
            (pluso x y z)]
           [(znego x) (znonnego y)
            (fresh (x-abs)
              (abso x x-abs)
              (conde [(== x-abs y)
                      (zeroo z)]
                     [(<o x-abs y)
                      (minuso y x-abs z)]
                     [(<o y x-abs)
                      (fresh (z-abs)
                        (minuso x-abs y z-abs)
                        (== z `(- . ,z-abs)))]))]
           [(znonnego x) (znego y)
            (zpluso y x z)]
           )))

(define rember
  ;; If x is in l, remove the first occurence of x in l -> res
  ;; If x is not in l, res is #f
  (lambda (x l res)
    (conde [(== l '()) (== res #f)]
           [(== l `(,x . ,res))]
           [(fresh (la ld recur)
              (== l `(,la . ,ld))
              (=/= x la)
              (rember x ld recur)
              (conde [(== recur #f) (== res #f)]
                     [(=/= recur #f) (== res `(,la . ,recur))]))])))

(define overlaps
  ;; Just return the list of duplicates
  (lambda (l o)
    (conde [(== l '()) (== o '())]
           [(fresh (a d)
              (== l `(,a . ,d))
              (conde [(rember a d #f)
                      (overlaps d o)]
                     [(fresh (l+ od)
                        (rember a d l+)
                        (=/= l+ #f)
                        (== o `(,a . ,od))
                        (overlaps l+ od))]))])))

;; shape is a list of list of points
;; a point is a list of four coordinates (numbers)
(define shift-point
  (lambda (p right up p+)
    (fresh (x1 y1 x2 y2
               x1+ y1+ x2+ y2+)
      (== p  `[,x1 ,y1 ,x2 ,y2])
      (== p+ `[,x1+ ,y1+ ,x2+ ,y2+])
      (zpluso x1 right x1+) (zpluso x2 right x2+)
      (zpluso y1 up y1+)    (zpluso y2 up y2+))))

(define mapo
  (lambda (f l out)
    (conde [(== l '()) (== out '())]
           [(fresh (a d fa d-out)
              (== l `(,a . ,d))
              (f a fa)
              (== out `(,fa . ,d-out))
              (mapo f d d-out))])))

(define shift-shape
  (lambda (shape right up shape+)
    (mapo (lambda (p p+) (shift-point p right up p+))
          shape
          shape+)))

(define shift-shape-freely
  (lambda (shape shape+)
    (fresh (right up)
      (integer right) (integer up)
      (shift-shape shape right up shape+))))

(define shapes-overlaps
  (lambda (s1 s2 o)
    (fresh (s12)
      (appendo s1 s2 s12)
      (overlaps s12 o))))

(define shapes-shift-overlaps
  (lambda (s1 s2 o)
    (fresh (s1+)
      (shift-shape-freely s1 s1+)
      (shapes-overlaps s1+ s2 o))))
