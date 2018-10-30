(import (kara-lang main)
        (mol))

(load "types.ss")

;;; Assignment syntax
(define-syntax up!
  (syntax-rules ()
    [(_ iden more ...)
     (set! iden (up iden more ...))]))

;;; Test zone
(define (intro)
  (define root new-var)
  (up! root '[] '(f 0))
  (up! root '[0] '(A))
  (assert (not (up root '[0] '(NOT-A))))
  )
(intro)

(define (sync)
  (define root '(f 0 1))
  (up! root '[0] '(f 0 1 2))
  (up! root '[1] '(f 0 1 2))
  (up! root '[] '(f 0 0))
  (assert (equal? (ref root '[0])
                  (ref root '[1])))
  (up! root '[0 0] '(B))
  (assert (equal? (ref root '[1 0])
                  '(B)))
  (up! root '[1 2] '(C))
  (assert (equal? (ref root '[0 2])
                  (ref root '[1 2])))
  (assert (not (up root '[1 2] '(D))))
  )
(sync)

(define (rep)
  (define root '(f 0 (f 6) 2 3 4 5))
  (up! root '[] '(f 0 (f 0) 2 3 4 5))
  (up! root '[2] '(->))
  (up! root '[] '(f 0 1 2 3 3 4))
  (pydisplay "0 = 1-0; 3 = 4")
  (pydisplay root)
  )
(rep)

(define (sync)
  (define root '(f 0 1 2))
  (up! root '[0] '(A))
  (up! root '[] '(f 0 0 1))
  (assert (equal? (ref root '[1])
                  '(A)))
  (up! root '[2] '(B))
  (assert (not (up root '[] '(f 0 1 0))))
  )
(sync)

(define (cyclic)
  (define root '(f 0 1 2))
  (up! root '[0] '(f 0))
  (up! root '[2] '(f 0))
  (up! root '[] '(f (f 0) 0 1))
  (assert (not (up root '[] '(f 0 0 1))))
  )
(cyclic)

(define (inter-root)
  (newline)
  (let ([get-modelel1  '(f 0 0 1 2)]
        [get-modelel2  '(f 0 1 1 2)]
        [root          new-var])
    (up! root '[] get-modelel1)
    (up! root '[] get-modelel2)
    (up! root '[] '(f 0 1 2 2))
    (pydisplay "0 = 1 = 2 = 3")
    (pydisplay root))
  )
(inter-root)

(define (inter-cycle)
  (define r1 '(f 0 0))
  (define r2 '(f 0 1))
  (up! r2 '[0] '(f 0))
  (up! r2 '[] '(f (f 0 1) 0))
  (assert (not (up r2 '[] r1)))
  (assert (not (up r1 '[] r2)))
  )
(inter-cycle)

(define (inter-no-cycle)
  (define r1 '(f 0 0 1))
  (define r2 '(f 0 1 (f 0)))
  (up! r2 '[] '(f 0 1 (f 2 1)))
  (newline) (pydisplay "0 = 1 = 2-0")
  (pydisplay (up r2 '[] r1))
  )
(inter-no-cycle)

(define (inter-advanced)
  (let* ([mod (up '(f 0 1) '[1] '(f 0))]
         [mod (up mod '[] '(f 0 (f 0)))]
         [root '(f 0 1)])
    (up! root '[0] '(N))
    (up! root '[] mod)
    (assert (equal? (ref root '[1 0])
                    '(N))))
  )
(inter-advanced)

(define (advanced-stuff)
  (newline)
  (let* ([rt p]
         [rt (up rt '[0] i)]
         [rt (up rt '[1] k)])
    (pydisplay "Conclusion says (-> A (-> B A))")
    (pydisplay (ref rt '[2])))

  (let* ([rt2 p]
         [rt2 (up rt2 '[0] k)]
         [rt2 (up rt2 '[1] i)])
    (newline)
    (pydisplay "Conclusion says (-> A (-> B B))")
    (pydisplay (ref rt2 '[2])))
  )
(advanced-stuff)

(define (tricky-topology)
  (let* ([root '(f 0 1 2)]
         [root (up root '[0] '(f 0 1))]
         [root (up root '[0 0] '(f 0))]
         [root (up root '[1] '(f 0 1))]
         [root (up root '[] '(f (f (f 0) 2) 1 0))])
    (assert (equal? (ref root '[0 0 0])
                    (ref root '[2])))

    (up! root '[] '(f 0 (f 1 1) 2))
    (up! root '[] '(f 0 0 1))
    (assert (equal? (ref root '[0 0 0])
                    (ref root '[2]))))
  )
(tricky-topology)

(define (obvious)
  (assert (up '(-> 0 0) '[] '(-> 0 0))))
(obvious)
