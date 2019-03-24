(define =r (lambda (x =xx) (== `(= ,x ,x) =xx)))
(define =s (lambda (=yx =xy) (fresh (x y) (== `(= ,y ,x) =yx) (== `(= ,x ,y) =xy))))
(define =t
  (lambda (=xy =yz =xz)
    (fresh (x y z)
      (== `(= ,y ,x) =xy)
      (== `(= ,y ,z) =yz)
      (== `(= ,x ,z) =xz))))

;; x = y and z = y -> z = x
(define proof1
  (lambda (=xy =zy =zx)
    (fresh (=yx)
      (=s =xy =yx)
      (=t =zy =yx =zx))))

(define man-mortal
  (lambda (is-man is-mortal)
    (fresh (man)
      (== `(is ,man Man) is-man)
      (== `(is ,man Mortal) is-mortal))))

(define socrates-mortal (lambda (out) (man-mortal `(is socrates Man) out)))

(define is-Nat (lambda (n) (conde [(== 0 n)] [(fresh (m) (== `(S ,m) n) (is-Nat m))])))
(define nat-rec
  (lambda (n base step P)
    (fresh ()
      (P 0 base)
      (eigen (m)
        (fresh (Pm Psm)
          (P m Pm)
          (P `(S ,m) Psm)
          (step Pm Psm))))))
(define plus
  (lambda (n m P)
    (= `(+ ,n ,m) )))
