(define fish1
  (map (lambda (c) (map build-num c))
       '([0 0 1 0] [0 1 1 1] [1 0 1 1] [1 0 2 0] [1 1 1 2] [1 1 2 1] [2 0 2 1] [2 1 2 2])))

(define fish2
  (map (lambda (c) (map build-num c))
       '([0 0 0 1] [0 1 0 2] [0 1 1 1] [0 2 1 2] [1 0 1 1] [1 1 1 2] [1 1 2 1] [1 2 2 2])))

(define pyramid1
  (map (lambda (c) (map build-num c))
       '([2 2 2 2]
         [1 1 1 1] [3 1 3 1]
         [0 0 0 0] [2 0 2 0] [4 0 4 0])))

(define pyramid2
  (map (lambda (c) (map build-num c))
       '([0 2 0 2] [2 2 2 2] [4 2 4 2]
         [1 1 1 1] [3 1 3 1]
         [2 0 2 0])))

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

(define zbuild-num
  (lambda (z)
    (if (nonnegative? z) (build-num z)
        (cons '- (build-num (abs z))))))

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
            (zpluso y x z)])))

(define z<=o
  (lambda (x y)
    (fresh (z)
      (znonnego z)
      (zpluso x z y))))

(define z<o
  (lambda (x y)
    (zposo y)
    (fresh (z)
      (zposo z)
      (zpluso x z y))))

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
           [(fresh (a d fa out-d)
              (== l `(,a . ,d))
              (f a fa)
              (== out `(,fa . ,out-d))
              (mapo f d out-d))])))

(define fold-righto
  (lambda (f init l out)
    (conde [(== l '()) (== out init)]
           [(fresh (a d b)
              (== l `(,a . ,d))
              (fold-righto f init d b)
              (f a b out))])))

(define shift-shape
  (lambda (shape right up shape+)
    (mapo (lambda (p p+) (shift-point p right up p+))
          shape shape+)))

(define shapes-overlaps
  (lambda (s1 s2 o)
    (fresh (s12)
      (appendo s1 s2 s12)
      (overlaps s12 o))))

(define mino
  (lambda (ls min)
    (fresh (a d)
      (== ls `(,a . ,d))
      (fold-righto (lambda (x y z)
                     (conde [(z<=o x y) (== z x)]
                            [(z<o y x)  (== z y)]))
                   a d min))))

(define maxo
  ;; returns #f if the list is empty
  (lambda (ls max)
    (fresh (a d)
      (== ls `(,a . ,d))
      (fold-righto (lambda (x y z)
                     (conde [(z<=o x y) (== z y)]
                            [(z<o y x)  (== z x)]))
                   a d max))))

(define shape-boundary-core
  (lambda (shape bottom   top   left   right
            bottom++ top++ left++ right++)
    (conde [(== shape '())
            (== bottom bottom++) (== top top++) (== left left++) (== right right++)]
           [(fresh (x1 y1 x2 y2 point d)
              (== shape `([,x1 ,y1 ,x2 ,y2] . ,d))
              (fresh (bottom+ top+ left+ right+)
                (mino `(,bottom ,y1 ,y2) bottom+) (maxo `(,top ,y1 ,y2)   top+)
                (mino `(,left ,x1 ,x2)   left+)   (maxo `(,right ,x1 ,x2) right+)
                (shape-boundary-core d
                                     bottom+  top+  left+  right+
                                     bottom++ top++ left++ right++)))])))

(define shape-boundary
  (lambda (shape bottom+ top+ left+ right+)
    (fresh (x1 x2 y1 y2 d bottom top left right)
      (== shape `([,x1 ,y1 ,x2 ,y2] . ,d))
      (mino `(,y1 ,y2) bottom) (maxo `(,y1 ,y2) top)
      (mino `(,x1 ,x2) left)   (maxo `(,x1 ,x2) right)
      (shape-boundary-core shape
                           bottom  top  left  right
                           bottom+ top+ left+ right+))))

(define shapes-shift-overlaps
  (lambda (s1 s2 o)
    (fresh (s1-bottom s1-top s1-left s1-right
                      s2-bottom s2-top s2-left s2-right
                      s1-height s1-width
                      s2-height s2-width)
      (shape-boundary s1 s1-bottom s1-top s1-left s1-right)
      (shape-boundary s2 s2-bottom s2-top s2-left s2-right)
      (zpluso s1-bottom s1-height s1-top)
      (zpluso s2-bottom s2-height s2-top)
      (fresh (up right s1+)
        ;; up    in [- s1-height, s2-height]
        ;; right in [- s1-width, s2-width]
        (z<o  `(- . ,s1-height) up)
        (z<o  `(- . ,s1-width)  right)
        (z<o up    s2-height)
        (z<o right s2-width)
        (shift-shape s1 right up s1+)
        (shapes-overlaps s1+ s2 o)))))
