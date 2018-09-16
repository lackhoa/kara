#! /usr/bin/racket
#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         rackunit)

(def (dmr root mol)
  (displayln (mol-repr root mol)))

(test-case
 "Introduction"
 (def root (make-root))
 (set! root (update root '[0] 'I))
 (set! root (update root '[0 0] 'A))
 (check-eq? (update root '[0 0] 'NOT-A)
            'conflict))

(test-case
 "Update working with sync"
 (def root (make-root))
 (set! root (update root '[0]))
 (set! root (update root '[1]))
 (set! root (sync root '[0] '[1]))
 (set! root (update root '[0] 'A))
 (check-eq? (ref-data root '[0])
            (ref-data root '[1]))
 (set! root (update root '[0 0] 'B))
 (check-eq? (ref-data root '[0 0])
            (ref-data root '[1 0]))
 (set! root (update root '[1 2] 'C))
 (check-eq? (ref-data root '[0 2])
            (ref-data root '[1 2]))
 (check-eq? (update root '[1 2] 'D)
            'conflict))

(test-case
 "Representation"
 (def root (make-root))
 (set! root (update root '[0]))
 (set! root (update root '[1 0]))
 (set! root (sync root '[0] '[1 0]))
 (set! root (update root '[2] '->))
 (set! root (update root '[3]))
 (set! root (update root '[4]))
 (set! root (sync root '[3] '[4]))
 (set! root (update root '[5]))
 (displayln "0, 1-0 and 3, 4 should be the same")
 (dmr root root))

(test-case
 "Synchronization"
 (def root (make-root))
 (set! root (update root '[0] 'A))
 (set! root (sync root '[0] '[1]))
 (check-eq? (ref-data root '[0])
            (ref-data root '[1]))

 (set! root (update root '[2] 'B))
 (check-eq? 'conflict
            (sync root '[0] '[2])))

(test-case
 "Cyclical synchronization"
 (def root (make-root))
 (set! root (update root '[0 0]))
 (set! root (sync root '[0 0] '[1]))
 (check-eq? 'conflict
            (sync root '[0] '[1]))
 (check-eq? 'conflict
            (sync root '[2] '[2 3 4])))

(test-case
 "Cloning"
 (def root (make-root))
 (set! root (update root '[0] 'K))
 (set! root (update root '[0 0] 'L))
 (set! root (sync root '[1] '[0 0]))
 (set! root (copy root '[0] '[2]))
 (dmr root root))
