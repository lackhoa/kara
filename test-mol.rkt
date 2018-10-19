#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "types.rkt"
         rackunit)

;;; Assignment (NOT mutation)
(define-syntax-rule (msync! iden more ...)
  (set! iden (msync iden more ...)))

(define-syntax-rule (up! iden more ...)
  (set! iden (up iden more ...)))



;;; Test zone
(test-case
 "Introduction"
 (def root new-var)
 (up! root '[] (get-model 1))
 (up! root '[0] '(mol% A))
 (check-false (up root '[0] '(mol% NOT-A)))
 )

(test-case
 "Update working with sync"
 (def root (get-model 2))
 (up! root '[0] (get-model 3))
 (up! root '[1] (get-model 3))
 (msync! root '[0] '[1])
 (up! root '[0] '(var% 0))
 (check-equal? (ref root '[0])
               (ref root '[1]))
 (up! root '[0 0] '(mol% B))
 (check-equal? (ref root '[1 0])
               '(mol% B))
 (up! root '[1 2] '(mol% C))
 (check-equal? (ref root '[0 2])
               (ref root '[1 2]))
 (check-false (up root '[1 2] '(mol% D)))
 )

(test-case
 "Representation"
 (def root (get-model 6))
 (up! root '[1] (get-model 1))
 (msync! root '[0] '[1 0])
 (up! root '[2] '(mol% ->))
 (msync! root '[3] '[4])
 (displayln "0 = 1-0; 3 = 4")
 (pdisplay root)
 )

(test-case
 "Synchronization"
 (def root (get-model 3))
 (up! root '[0] '(mol% A))
 (msync! root '[0] '[1])
 (check-equal? (ref root '[1])
               '(mol% A))
 (up! root '[2] '(mol% B))
 (check-false (msync root '[0] '[2]))
 )

(test-case
 "Cyclical synchronization"
 (def root (get-model 3))
 (up! root '[0] (get-model 1))
 (up! root '[2] (get-model 1))
 (msync! root '[0 0] '[1])
 (check-false (msync root '[0] '[1]))
 (check-false (msync root '[2] '[2 0]))
 )

(test-case
 "Inter-root synchronization"
 (newline)
 (def get-modelel1 (msync (get-model 4) '[0] '[1]))
 (def get-modelel2 (msync (get-model 4) '[1] '[2]))
 (def root new-var)
 (up! root '[] get-modelel1)
 (up! root '[] get-modelel2)
 (msync! root '[2] '[3])
 (displayln "0 = 1 = 2 = 3")
 (pdisplay root)
 )

(test-case
 "Inter-root cycle"
 (def r1 (get-model 2))
 (msync! r1 '[0] '[1])

 (def r2 (get-model 2))
 (up! r2 '[0] (get-model 1))
 (msync! r2 '[1] '[0 0])
 (check-false (up r2 '[] r1))
 (check-false (up r1 '[] r2))
 )

(test-case
 "Inter-root without cycle"
 (def r1 (get-model 3))
 (msync! r1 '[0] '[1])

 (def r2 (get-model 3))
 (up! r2 '[2] (get-model 1))
 (msync! r2 '[1] '[2 0])
 (newline) (displayln "0 = 1 = 2-0")
 (pdisplay (up r2 '[] r1))
 )

(test-case
 "Inter-root advanced"
 (def get-modelel (up (get-model 2) '[1] (get-model 1)))
 (msync! get-modelel '[0] '[1 0])

 (def root (get-model 2))
 (up! root '[0] '(mol% N))
 (up! root '[] get-modelel)
 (check-equal? (ref root '[1 0])
               '(mol% N)))

(test-case
 "Advanced stuff"
 (newline)
 (def rt p)
 (up! rt '[0] i) (up! rt '[1] k)
 (displayln "Conclusion says (-> A (-> B A))")
 (displayln (ref rt '[2]))

 (def rt2 p)
 (up! rt2 '[0] k) (up! rt2 '[1] i)
 (newline) (displayln "Conclusion says (-> A (-> B B))")
 (displayln (ref rt2 '[2]))
 )

(test-case
 "Tricky topology"
 (def root (get-model 3))
 (up! root '[0] (get-model 1))
 (up! root '[0 0] (get-model 1))
 (up! root '[1] (get-model 2))
 (msync! root '[0 0 0] '[2])
 (check-equal? (ref root '[0 0 0])
               (ref root '[2]))

 (msync! root '[1 0] '[1 1])
 (msync! root '[0] '[1])
 (check-equal? (ref root '[0 0 0])
               (ref root '[2]))
 )
