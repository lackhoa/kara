(define nullo
  (lambda (x) (== x '[])))

(define pairo
  (lambda (x) (fresh (a d) (== x (cons a d)))))

(define atomo
  (lambda (x) (conde [(nullo x)]
                [(pairo x)])))

(define cro
  (lambda (x x-car x-cdr)
    (== x `[,x-car . ,x-cdr])))

(define caro
  (lambda (x x-car)
    (fresh (x-cdr)
      (cro x x-car x-cdr))))

(define cdro
  (lambda (x x-cdr)
    (fresh (x-car)
      (cro x x-car x-cdr))))

(define reflect
  (lambda (x)
    (project (x)
      (begin (display x) (newline)
             succeed))))

(define select
  (lambda (x ls res)
    (fresh (a d)
      (== ls (cons a d))
      (conde [(== x a)  (== res d)]
             [(fresh (resd)
                (== res (cons a resd))
                (select x d resd))]))))

(define select-many
  (lambda (l1 l l2)
    (conde [(nullo l1)  (== l l2)]
           [(fresh (x l1d ld)
              (cro l1 x l1d)
              (cro l  x ld)
              (select-many l1d ld l2))]
           [(fresh (x y l1d l2d ld)
              (cro l1 x l1d)
              (cro l2 y l2d)
              (cro l y ld)
              (select-many (cons x l1d) ld l2d))])))

(define appendo
  (lambda (l1 l2 l)
    (conde [(nullo l1)  (== l l2)]
           [(fresh (l1a l1d ld)
              (cro l1 l1a l1d)
              (cro l l1a ld)
              (appendo l1d l2 ld))])))

(define appendo*
  (lambda (l* l)
    (conde [(nullo l*)  (nullo l)]
           [(fresh (x)
              (== l* (list x))
              (== l x))]
           [(fresh (l*a l*b l*d l*bd)
              (== l* `[,l*a ,l*b . ,l*d])
              (appendo l*a l*bd l)
              (appendo* (cons l*b l*d) l*bd))])))


(define list-splito-core
  (lambda (l1^ l2^ n^ l1 l2)
    (conde [(== n^ (build-num 0))
            (reverseo l1^ l1)
            (== l2^ l2)]

           [(fresh (l2^car l2^cdr n)
              (;; This must be first, otherwise predo
               ;; can keep running forever
               cro l2^ l2^car l2^cdr)
              (predo n^ n)
              (list-splito-core `[,l2^car . ,l1^]
                                l2^cdr n l1 l2))])))

(define list-splito
  (lambda (l n l1 l2)
    (list-splito-core '[] l n l1 l2)))

(define list-heado
  (lambda (l n l1)
    (fresh (l2)
      (list-splito l n l1 l2))))

(define list-tailo
  (lambda (l n l2)
    (fresh (l1)
      (list-splito l n l1 l2))))

(define list-refo
  (lambda (l n x)
    (fresh (l1)
      (list-tailo l n l1)
      (caro l1 x))))

(define cadro   (lambda (l x) (list-refo l (build-num 1) x)))
(define caddro  (lambda (l x) (list-refo l (build-num 2) x)))
(define cadddro (lambda (l x) (list-refo l (build-num 3) x)))

(define cddro   (lambda (l^ l) (list-tailo l^ (build-num 2) l)))
(define cdddro  (lambda (l^ l) (list-tailo l^ (build-num 3) l)))
(define cddddro (lambda (l^ l) (list-tailo l^ (build-num 4) l)))

(define lengtho
  (lambda (ls len)
    (conde [(nullo ls)  (zeroo len)]
           [(fresh (x xs n)
              (pluso n (build-num 1) len)
              (cro ls x xs)
              (lengtho xs n))])))

(define membero
  (lambda (mem ls)
    (fresh (a d)
      (== ls `(,a . ,d))
      (conde [(== a mem)]
             [(=/= a mem) (membero mem d)]))))

(define reverseo
  (lambda (x y)
    (reverseo-core x '[] y)))

(define reverseo-core
  (lambda (x o y)
    (conde [(nullo x) (== y o)]
           [(fresh (x-car x-cdr)
              (cro x x-car x-cdr)
              (reverseo-core x-cdr `[,x-car . ,o] y))])))

(define half-halfo
  (lambda (l l1 l2)
    (conde [(nullo l)  (nullo l1) (nullo l2)]
           [(fresh (la ld l1d)
              (cro l la ld)
              (cro l1 la l1d)
              (half-halfo ld l2 l1d))])))

(define key-mergeo
  (lambda (x y z)
    (conde [(nullo x) (== y z)]
           [(pairo x) (nullo y) (== x z)]
           [(fresh (xa xk xd ya yk yd za zd)
              (cro x xa xd) (cro y ya yd)
              (caro xa xk)  (caro ya yk)
              (cro z za zd)
              (conde [(<=o xk yk)
                      (key-mergeo xd y zd)
                      (== za xa)]
                     [(<o yk xk)
                      (key-mergeo x yd zd)
                      (== za ya)]))])))

(define key-sorto
  (lambda (l^ l)
    (conde [(conde [(nullo l^)]
                   [(fresh (l^d)
                      (cdro l^ l^d) (nullo l^d))])
            (== l l^)]
           [(fresh (_0)
              (cddro l^ _0))
            (fresh (l^1 l^2 l1 l2)
              (half-halfo l^ l^1 l^2)
              (key-sorto l^1 l1)
              (key-sorto l^2 l2)
              (key-mergeo l1 l2 l))])))

(define rembero
  (lambda (x l res)
    (conde [(nullo l)  (nullo res)]
           [(fresh (ld)
              (cro l x ld)
              (rembero x ld res))]
           [(fresh (la ld resd)
              (cro l la ld)
              (=/= x la)
              (cro res la resd)
              (rembero x ld resd))])))





;;; Higher order stuff

(define composeo
  (lambda (f1 f2)
    (lambda (x z)
      (fresh (y) (f1 x y) (f2 y z)))))

(define-syntax naf
  (syntax-rules ()
    [(_ goal)
     (conda [goal fail]
            [succeed])]))

(define same-length
  (lambda (l1 l2)
    (conde [(nullo l1) (nullo l2)]
           [(fresh (a1 d1 a2 d2)
              (== l1 (cons a1 d1))
              (== l2 (cons a2 d2))
              (same-length d1 d2))])))

(define permute
  (lambda (l1 l2)
    (fresh ()
      (same-length l1 l2)
      (let loop ([l1 l1] [l2 l2])
        (conde [(nullo l1) (nullo l2)]
               [(fresh (x l1+ l2+)
                  (== l1 (cons x l1+))
                  (select x l2 l2+)
                  (loop l1+ l2+))])))))
