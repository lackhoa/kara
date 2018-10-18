#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         rackunit)

;;; Assignment (NOT mutation)
(define-syntax-rule (msync! iden more ...)
  (set! iden (msync iden more ...)))

(define-syntax-rule (pull! iden more ...)
  (set! iden (pull iden more ...)))


;;; Test zone
(test-case
 "Introduction"
 (def root new-var)
 (pull! root '[] '(mol% I (var% 0)))
 (pull! root '[0] '(mol% A))
 (check-false (pull root '[0] '(mol% NOT-A)))
 )

(test-case
 "Update working with sync"
 (def root new-var)
 (pull! root '[] '(mol% #f
                        (var% 3)
                        (mol% #f (var% 0) (var% 1) (var% 2))))
 ;; (msync! root '[0] '[1])
 ;; (pull! root '[0] '(var% 0))
 ;; (check-equal? (ref root '[0])
 ;;               (ref root '[1]))
 ;; (pull! root '[0] '(mol% #f (var% 0)))
 ;; (pull! root '[0 0] '(mol% B))
 ;; (check-equal? (ref root '[1 0])
 ;;               '(mol% B))
 ;; (pull! root '[1 2] '(mol% C))
 ;; (check-eq? (ref-data (dc root) '[0 2])
 ;;            'C)
 ;; (check-false (update (dc root) '[1 2] 'D))
 )

;; (test-case
;;  "Representation"
;;  (def root new-var)
;;  (pull! root '[0])
;;  (pull! root '[1 0])
;;  (msync! root '[0] '[1 0])
;;  (pull! root '[2] '->)
;;  (pull! root '[3])
;;  (pull! root '[4])
;;  (msync! root '[3] '[4])
;;  (pull! root '[5])
;;  (displayln "0 = 1-0; 3 = 4")
;;  (dm (dc root))
;;  )

;; (test-case
;;  "Synchronization"
;;  (def root new-var)
;;  (pull! root '[0] 'A)
;;  (msync! root '[0] '[1])
;;  (check-eq? (ref-data (dc root) '[1])
;;             'A)
;;  (pull! root '[2] 'B)
;;  (check-false (msync (dc root) '[0] '[2]))
;;  )

;; (test-case
;;  "Cyclical synchronization"
;;  (def root new-var)
;;  (pull! root '[0 0])
;;  (msync! root '[0 0] '[1])
;;  (check-false (msync (dc root) '[0] '[1]))
;;  (check-false (msync (dc root) '[2] '[2 3 4]))
;;  )

;; (test-case
;;  "Inter-root synchronization"
;;  (newline)
;;  (def model1 (msync new-var '[0] '[1]))
;;  (def model2 (msync new-var '[1] '[2]))
;;  (def r (update new-var '[0] 'T))
;;  (pull! r '[] model1)
;;  (pull! r '[] model2)
;;  (msync! r '[2] '[3])
;;  (displayln "0 = 1 = 2 = 3")
;;  (dm (dc r))
;;  )

;; (test-case
;;  "Inter-root cycle"
;;  (def r1 new-var)
;;  (msync! r1 '[0] '[1])

;;  (def r2 new-var)
;;  (msync! r2 '[1] '[0 0])
;;  (check-false (pull (dc r2) '[] r1))
;;  (check-false (pull (dc r1) '[] r2))
;;  )

;; (test-case
;;  "Inter-root without cycle"
;;  (def r1 new-var)
;;  (msync! r1 '[0] '[1])

;;  (def r2 new-var)
;;  (msync! r2 '[1] '[2 0])
;;  (newline) (displayln "0 = 1 = 2-0")
;;  (dm (pull r2 '[] r1))
;;  )

;; (test-case
;;  "Inter-root advanced"
;;  (def model new-var)
;;  (msync! model '[0] '[1 0])

;;  (def r new-var)
;;  (pull! r '[0] 'N)
;;  (pull! r '[] model)
;;  (check-eq? (ref-data (dc r) '[1 0])
;;             'N))

;; (test-case
;;  "Advanced shit"
;;  (def sy msync)
;;  (def up update)
;;  (def ai (sy (up (up new-var '[] 'ai=>) '[0] '->) '[0 0] '[0 1]))

;;  (def ak (sy (up (up (up new-var '[] 'ak=>) '[0] '->) '[0 1] '->) '[0 0] '[0 1 1]))

;;  (def mp (sy (sy (up (up new-var '[]  'mp=>) '[1 0]  '->) '[0]  '[1 0 1]) '[1 0 0]  '[2 0]))

;;  (newline)
;;  (def rt mp)
;;  (set! rt (pull rt '[1] ai))
;;  (displayln "Conclusion says (-> A (-> B A))")
;;  (dm (detach (pull (dc rt) '[2] ak) '[0]))

;;  (def rt2 mp)
;;  (set! rt2 (pull (dc rt2) '[1] ak))
;;  (newline) (displayln "Conclusion says (-> A (-> B B))")
;;  (dm (detach (pull (dc rt2) '[2] ai) '[0]))
;;  )

;; (test-case
;;  "Tricky topology"
;;  (def m new-var)
;;  (msync! m '[0 0 0] '[2])
;;  (check-not-false (member '[2]
;;                           (ref-sync (dc m) '[0 0 0])))

;;  (msync! m '[1 0] '[1 1])
;;  (msync! m '[0] '[1])
;;  (check-not-false (member '[2]
;;                           (ref-sync (dc m) '[0 0 0])))
;;  )
