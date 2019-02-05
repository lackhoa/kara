(define fish1
  '([0 0 1 0]
    [0 1 1 1]
    [1 0 1 1]
    [1 0 2 0]
    [1 1 1 2]
    [1 1 2 1]
    [2 0 2 1]
    [2 1 2 2]))

(define fish2
  '([0 0 0 1]
    [0 1 0 2]
    [0 1 1 1]
    [0 2 1 2]
    [1 0 1 1]
    [1 1 1 2]
    [1 1 2 1]
    [1 2 2 2]))

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

;; (define translate
;;   (lambda (s s+)
;;     ))
