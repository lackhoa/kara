(import (chezscheme) (kara-lang main)
        (types) (enum))
(load "mol.ss")

(define (trivial)
  (assert (not (instance? (up new-var '[] '(a))
                        (up new-var '[] '(b)))))
  (assert (instance? new-var new-var))
  )
(trivial)

(define (generality)
  (assert (not (instance? new-var '(-> 0 1))))
  (assert (instance? '(-> 0 1) '(-> 0 1)))

  (assert (eq? (instance? (up (up (up '(f 0 1) '[0] '(-> 0 1))
                                  '[1] '(-> 0 1))
                              '[] '(f 0 0))
                          (up '(f 0 1) '[] '(f 0 0)))
               #t))
  (let ([base '(f 0 0 1 1)])
    (assert (not (instance? base
                          (up base '[] '(f 0 1 1 2)))))
    (assert (instance? (up base '[] '(f 0 1 1 2))
                       base)))
  )
(generality)

(define (multi-leveled)
  (let ([r  '(-> (-> (-> 0 1) (-> 0 1))
                (-> (-> 0 1) (-> 0 1)))])
    (assert (instance? r i))
    (assert (not (instance? i r))))
  )
(multi-leveled)

(define (ultimate)
  (let ([r  '(-> (-> 0 1)
                (-> 2 (-> 0 1)))])

    (assert (instance? r k) )
    (assert (not (instance? k r)) )
    (assert (< (size k) (size r)) ))
  )
(ultimate)

(define (edgy)
  (let ([r1  '(-> (-> 0 1) (-> 1 0))]
        [r2  '(-> (-> 0 1) (-> 0 1))])
    (assert (not (instance? r2 r1)))

    (let([r3  '(-> (-> 0 1) (-> 2 0))])
      (assert (not (instance? r2 r3)))
      (assert (instance? r1 r3))))
  )
(edgy)
