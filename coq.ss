(load "miniKanren/mk.scm")
(define subo
  (lambda (x v f g)
    (let subo ([f f] [g g])
      (conde
       [(== x f) (== v g)]
       [(=/= x f)
        (conde
         [(conde [(== `() f)] [(numbero f)] [(symbolo f)])
          (== f g)]
         [(fresh (fa fd ga gd)
            (== `(,fa . ,fd) f)
            (== `(,ga . ,gd) g)
            (subo fa ga)
            (subo fd gd))])]))))

(define indo
  (lambda (n base step P)
    (fresh (x)
      (subo x 0 P base))
    (fresh (x m)
      (== step `(-> ,Pm ,Psm))
      (subo x m P Pm)
      (subo x `(S ,m) Psm))))
