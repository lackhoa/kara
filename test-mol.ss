(import (kara-lang main))

(load "types.ss")
(load "mol.ss")

;;; Assignment syntax
(define-syntax up!
  (syntax-rules ()
    [(_ iden more ...)
     (set! iden (up iden more ...))]))

;;; Test zone
(define (intro)
  (define root new-var)
  (up! root '[] '(0))
  (up! root '[car] 'A)
  (assert (not (up root '[car] 'NOT-A))))
(intro)

(define (sync-test)
  (define root '(0 1))
  (up! root '[car] '(0 1 2))
  (up! root '[cdr car] '(0 1 2))
  (up! root '[] '(0 0))
  (assert (equal? (ref root '[car])
                  (ref root '[cdr car])))
  (up! root '[car car] 'B)
  (assert (equal? (ref root '[cdr car car])
                  'B))
  (up! root '[cdr car cdr cdr car] '(C))
  (assert (equal? (ref root '[car cdr cdr car])
                  (ref root '[cdr car cdr cdr car])))
  (assert (not (up root '[cdr car cdr cdr car] '(D))))
  )
(sync-test)

(define (rep)
  (define root '(0 (6) 2 3 4 5))
  (up! root '[] '(0 (0) 2 3 4 5))
  (up! root '[cdr cdr car] '->)
  (up! root '[] '(0 1 2 3 3 4))
  (pydisplay "0 = 1-0; 3 = 4")
  (pydisplay root)
  )
(rep)

(define (sync-more)
  (define root '(0 1 2))
  (up! root '[car] 'A)
  (up! root '[] '(0 0 1))
  (assert (equal? (ref root '[cdr car])
                  'A))
  (up! root '[cdr cdr car] 'B)
  (assert (not (up root '[] '(0 1 0))))
  )
(sync-more)

(define (cyclic)
  (define root '(0 1 2))
  (up! root '[car] '(0))
  (up! root '[cdr cdr car] '(0))
  (up! root '[] '((0) 0 1))
  (assert (not (up root '[] '(0 0 1))))
  )
(cyclic)

(define (inter-root)
  (newline)
  (let ([get-modelel1  '(0 0 1 2)]
        [get-modelel2  '(0 1 1 2)]
        [root          new-var])
    (up! root '[] get-modelel1)
    (up! root '[] get-modelel2)
    (up! root '[] '(0 1 2 2))
    (pydisplay "0 = 1 = 2 = 3")
    (pydisplay root))
  )
(inter-root)

(define (inter-cycle)
  (define r1 '(0 0))
  (define r2 '(0 1))
  (up! r2 '[] '((0 1) 0))
  (assert (not (up r2 '[] r1)))
  (assert (not (up r1 '[] r2)))
  )
(inter-cycle)

(define (inter-no-cycle)
  (define r1 '(0 0 1))
  (define r2 '(0 1 (1 2)))
  (newline) (pydisplay "0 = 1 = 2-0")
  (pydisplay (up r2 '[] r1))
  )
(inter-no-cycle)

(define (inter-advanced)
  (let* ([mod (up '(0 1) '[cdr car] '(0))]
         [mod (up mod '[] '(0 (0)))]
         [root '(0 1)])
    (up! root '[car] '(N))
    (up! root '[] mod)
    (assert (equal? (ref root '[cdr car car])
                    '(N))))
  )
(inter-advanced)

(define (advanced-stuff)
  (newline)
  (let* ([rt p]
         [rt (up rt '[cdr car] i)]
         [rt (up rt '[cdr cdr car] k)])
    (pydisplay "Conclusion says (-> A (-> B A))")
    (pydisplay (ref rt '[cdr cdr cdr car])))

  (let* ([rt2 p]
         [rt2 (up rt2 '[cdr car] k)]
         [rt2 (up rt2 '[cdr cdr car] i)])
    (newline)
    (pydisplay "Conclusion says (-> A (-> B B))")
    (pydisplay (ref rt2 '[cdr cdr cdr car])))
  )
(advanced-stuff)

(define (tricky-topology)
  (let* ([root '(0 1 2)]
         [root (up root '[car] '(0 1))]
         [root (up root '[car car] '(0))]
         [root (up root '[cdr car] '(0 1))]
         [root (up root '[] '(((0) 2) 1 0))])
    (assert (equal? (ref root '[car car car])
                    (ref root '[cdr cdr car])))

    (up! root '[] '(0 (1 1) 2))
    (up! root '[] '(0 0 1))
    (assert (equal? (ref root '[car car car])
                    (ref root '[cdr cdr car]))))
  )
(tricky-topology)

(define (obvious)
  (assert (up '(-> 0 0) '[] '(-> 0 0))))
(obvious)

(define (pair)
  (newline)
  (pydisplay "Symmetric!")
  (pydisplay (up '(0 1 . (0 . 1)) '[cdr car] '()))
  (pydisplay "This says (a b c d)")
  (pydisplay (up '(0 1 . 2) '[] '(a b c d)))
  (assert (not (up '(0 1 2 . 3) '[] '(a b))))
  )
(pair)
