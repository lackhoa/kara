#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         rackunit)

;;; Preparation
(def dc
  (compose decompress compress))

;;; Assignment (NOT mutation)
(define-syntax-rule (update! iden more ...)
  (set! iden (update iden more ...)))

(define-syntax-rule (sync! iden more ...)
  (set! iden (msync iden more ...)))

(define-syntax-rule (pull! iden more ...)
  (set! iden (pull iden more ...)))


;;; Test zone
(test-case
 "Introduction"
 (def root new-root)
 (update! root '[] 'I)
 (update! root '[0] 'A)
 (check-false (update (dc root) '[0] 'NOT-A))
 )

(test-case
 "Update working with sync"
 (def root new-root)
 (sync! root '[0] '[1])
 (update! root '[0] 'A)
 (check-eq? (ref-data (dc root) '[1])
            'A)
 (update! root '[0 0] 'B)
 (check-eq? (ref-data (dc root) '[1 0])
            'B)
 (update! root '[1 2] 'C)
 (check-eq? (ref-data (dc root) '[0 2])
            'C)
 (check-false (update (dc root) '[1 2] 'D))
 )

(test-case
 "Representation"
 (def root new-root)
 (update! root '[0])
 (update! root '[1 0])
 (sync! root '[0] '[1 0])
 (update! root '[2] '->)
 (update! root '[3])
 (update! root '[4])
 (sync! root '[3] '[4])
 (update! root '[5])
 (displayln "0 = 1-0; 3 = 4")
 (dm (dc root))
 )

(test-case
 "Synchronization"
 (def root new-root)
 (update! root '[0] 'A)
 (sync! root '[0] '[1])
 (check-eq? (ref-data (dc root) '[1])
            'A)
 (update! root '[2] 'B)
 (check-false (msync (dc root) '[0] '[2]))
 )

(test-case
 "Cyclical synchronization"
 (def root new-root)
 (update! root '[0 0])
 (sync! root '[0 0] '[1])
 (check-false (msync (dc root) '[0] '[1]))
 (check-false (msync (dc root) '[2] '[2 3 4]))
 )

(test-case
 "Inter-root synchronization"
 (newline)
 (def model1 (msync new-root '[0] '[1]))
 (def model2 (msync new-root '[1] '[2]))
 (def r (update new-root '[0] 'T))
 (pull! r '[] model1)
 (pull! r '[] model2)
 (sync! r '[2] '[3])
 (displayln "0 = 1 = 2 = 3")
 (dm (dc r))
 )

(test-case
 "Inter-root cycle"
 (def r1 new-root)
 (sync! r1 '[0] '[1])

 (def r2 new-root)
 (sync! r2 '[1] '[0 0])
 (check-false (pull (dc r2) '[] r1))
 (check-false (pull (dc r1) '[] r2))
 )

(test-case
 "Inter-root without cycle"
 (def r1 new-root)
 (sync! r1 '[0] '[1])

 (def r2 new-root)
 (sync! r2 '[1] '[2 0])
 (newline) (displayln "0 = 1 = 2-0")
 (dm (pull r2 '[] r1))
 )

(test-case
 "Inter-root advanced"
 (def model new-root)
 (sync! model '[0] '[1 0])

 (def r new-root)
 (update! r '[0] 'N)
 (pull! r '[] model)
 (check-eq? (ref-data (dc r) '[1 0])
            'N))

(test-case
 "Advanced shit"
 (def sy msync)
 (def up update)
 (def ai (sy (up (up new-root '[] 'ai=>) '[0] '->) '[0 0] '[0 1]))

 (def ak (sy (up (up (up new-root '[] 'ak=>) '[0] '->) '[0 1] '->) '[0 0] '[0 1 1]))

 (def mp (sy (sy (up (up new-root '[]  'mp=>) '[1 0]  '->) '[0]  '[1 0 1]) '[1 0 0]  '[2 0]))

 (newline)
 (def rt mp)
 (set! rt (pull rt '[1] ai))
 (displayln "Conclusion says (-> A (-> B A))")
 (dm (detach (pull (dc rt) '[2] ak) '[0]))

 (def rt2 mp)
 (set! rt2 (pull (dc rt2) '[1] ak))
 (newline) (displayln "Conclusion says (-> A (-> B B))")
 (dm (detach (pull (dc rt2) '[2] ai) '[0]))
 )

(test-case
 "Tricky topology"
 (def m new-root)
 (sync! m '[0 0 0] '[2])
 (check-not-false (member '[2]
                          (ref-sync (dc m) '[0 0 0])))

 (sync! m '[1 0] '[1 1])
 (sync! m '[0] '[1])
 (check-not-false (member '[2]
                          (ref-sync (dc m) '[0 0 0])))
 )
