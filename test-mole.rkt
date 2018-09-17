#! /usr/bin/racket
#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt"
         rackunit)

(def (dm root)
  (displayln (mol-repr root)))

(test-case
 "Introduction"
 (def root (new-root))
 (set! root (update root '[0] 'I))
 (set! root (update root '[0 0] 'A))
 (check-eq? (update root '[0 0] 'NOT-A)
            'conflict))

(test-case
 "Update working with sync"
 (def root (new-root))
 (set! root (update root '[0]))
 (set! root (update root '[1]))
 (set! root (sync root '[0] '[1]))
 (set! root (update root '[0] 'A))
 (check-eq? (ref-data root '[1])
            'A)
 (set! root (update root '[0 0] 'B))
 (check-eq? (ref-data root '[1 0])
            'B)
 (set! root (update root '[1 2] 'C))
 (check-eq? (ref-data root '[0 2])
            'C)
 (check-eq? (update root '[1 2] 'D)
            'conflict))

(test-case
 "Representation"
 (def root (new-root))
 (set! root (update root '[0]))
 (set! root (update root '[1 0]))
 (set! root (sync root '[0] '[1 0]))
 (set! root (update root '[2] '->))
 (set! root (update root '[3]))
 (set! root (update root '[4]))
 (set! root (sync root '[3] '[4]))
 (set! root (update root '[5]))
 (displayln "0, 1-0 and 3, 4 are the same")
 (dm root))

(test-case
 "Synchronization"
 (def root (new-root))
 (set! root (update root '[0] 'A))
 (set! root (sync root '[0] '[1]))
 (check-eq? (ref-data root '[0])
            (ref-data root '[1]))

 (set! root (update root '[2] 'B))
 (check-eq? (sync root '[0] '[2])
            'conflict))

(test-case
 "Cyclical synchronization"
 (def root (new-root))
 (set! root (update root '[0 0]))
 (set! root (sync root '[0 0] '[1]))
 (check-eq? (sync root '[0] '[1])
            'conflict)
 (check-eq? (sync root '[2] '[2 3 4])
            'conflict))

(test-case
 "Inter-root synchronization"
 (def model1 (sync (new-root) '[0] '[1]))
 (def model2 (sync (new-root) '[1] '[2]))
 ;; (def model3 (update (new-root) '[] 'T))
 (def model3 (new-root))

 (def r (new-root))
 (set! r (pull r model1))
 (set! r (pull r model2))
 (set! r (pull r model3 '[1]))
 (set! r (sync r '[2] '[3]))
 (displayln "0, 1, 2 and 3 and are the same, with T being the data")
 (dm r))

(test-case
 "Inter-root cycle"
 (def r1 (new-root))
 (set! r1 (sync r1 '[0] '[1]))

 (def r2 (new-root))
 (set! r2 (sync r2 '[1] '[0 0]))
 (check-eq? (pull r2 r1 '[] '[])
            'conflict))

(test-case
 "Inter-root advanced"
 (def model (new-root))
 (set! model (sync model '[0] '[1 0]))

 (def r (new-root))
 (set! r (update r '[0] 'N))
 (set! r (pull r model '[] '[]))
 (check-eq? (ref-data r '[1 0])
            'N))

(test-case
 "Advanced shit"
 (newline)

 (def rt mp)
 (set! rt (pull rt ai '[1]))
 (displayln "Conclusion says (-> A (-> B A))")
 (dm (pull rt ak '[2]))

 (def rt2 mp)
 (set! rt2 (pull rt2 ak '[1]))
 (displayln "Conclusion says (-> A (-> B B))")
 (dm (pull rt2 ai '[2])))

(test-case
 "Advanced Pulling"
 (def r1 (new-root))

 (def r2 (new-root))
 (set! r2 (sync r2 '[0 0] '[0 1]))

 (newline)
 (displayln "0 and 1 are the same")
 (dm (pull r1 r2 '[] '[0])))
