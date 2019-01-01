(define nullo
  (lambda (x) (== x '[])))

(define pairo
  (lambda (x) (fresh (_0) (caro x _0))))

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

(define predo
  (lambda (n^ n)
    (pluso n (build-num 1) n^)))

(define succo
  (lambda (n^ n)
    (pluso n^ (build-num 1) n)))

(define-syntax reflect
  (syntax-rules ()
    [(_ x)
     (lambdag@ (a : B E S)
       (begin (display (walk* x S))
              (newline)
              a))]))

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
    (conde [(nullo ls) (zeroo len)]
           [(fresh (x xs n)
              (cro ls x xs)
              (lengtho xs n)
              (pluso n '[1] len))])))

(define membero
  (lambda (mem ls)
    (fresh (ls-car ls-cdr)
      (cro ls ls-car ls-cdr)
      (conde [(== ls-car mem)]
             [(membero mem ls-cdr)]))))

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

(define composeo
  (lambda (f1 f2)
    (lambda (x z)
      (fresh (y) (f1 x y) (f2 y z)))))

(define mapo
  ;; func needs to be a grounded
  ;; goal construct taking two arguments
  (lambda (func l^ l)
    (conde [(nullo l^) (nullo l)]
           [(fresh (l^a l^d ld v)
              (cro l^ l^a l^d)
              (func l^a v)
              (mapo func l^d ld)
              (cro l v ld))])))

(define-syntax naf
  (syntax-rules ()
    [(_ goal)
     (conda [goal fail]
            [succeed])]))
