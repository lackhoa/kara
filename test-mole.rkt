#! /usr/bin/racket
#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
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
 (def r1 (new-root))
 (set! r1 (sync r1 '[0] '[1]))

 (def r2 (new-root))
 (set! r2 (pull r2 r1 '[] '[]))
 (displayln "0 and 1 are the same")
 (dm r2))

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
