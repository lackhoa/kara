#! /usr/bin/racket
#lang racket
(require "lang/kara.rkt"
         "mole.rkt")

(test-case
 "Introduction"
 (def root (new-mol))
 (update! root '[] 'I)
 (update! root '[0] 'A)
 (check-false (update root '[0] 'NOT-A)))

(test-case
 "Update working with sync"
 (def root (new-mol))
 (sync! root '[0] '[1])
 (check-eq? (ref root '[0])
            (ref root '[1]))
 (update! root '[0] 'A)
 (check-eq? (ref-data root '[1])
            'A)
 (update! root '[0 0] 'B)
 (check-eq? (ref-data root '[1 0])
            'B)
 (update! root '[1 2] 'C)
 (check-eq? (ref-data root '[0 2])
            'C)
 (check-false (update root '[1 2] 'D))
 )

(test-case
 "Representation"
 (def root (new-mol))
 (update! root '[0])
 (update! root '[1 0])
 (sync! root '[0] '[1 0])
 (update! root '[2] '->)
 (update! root '[3])
 (update! root '[4])
 (sync! root '[3] '[4])
 (update! root '[5])
 (displayln "0, 1-0 and 3, 4 are the same")
 (dm root) (newline)
 )

(test-case
 "Synchronization"
 (def root (new-mol))
 (update! root '[0] 'A)
 (sync! root '[0] '[1])
 (check-eq? (ref-data root '[0])
            (ref-data root '[1]))

 (update! root '[2] 'B)
 (check-false (sync root '[0] '[2]))
 )

(test-case
 "Cyclical synchronization"
 (def root (new-mol))
 (update! root '[0 0])
 (sync! root '[0 0] '[1])
 (check-false (sync root '[0] '[1]))
 (check-false (sync root '[2] '[2 3 4]))
 )

(test-case
 "Inter-root synchronization"
 (def model1 (sync (new-mol) '[0] '[1]))
 (def model2 (sync (new-mol) '[1] '[2]))

 (def r (update (new-mol) '[0] 'T))
 (pull! r model1 '[])
 (pull! r model2 '[])
 (sync! r '[2] '[3])
 (displayln "0, 1, 2, 3 are the same")
 (dm r) (newline)
 )

(test-case
 "Inter-root cycle"
 (def r1 (new-mol))
 (sync! r1 '[0] '[1])

 (def r2 (new-mol))
 (sync! r2 '[1] '[0 0])
 (check-false (pull r2 r1 '[]))
 (check-false (pull r1 r2 '[]))
 )

(test-case
 "Inter-root without cycle"
 (def r1 (new-mol))
 (sync! r1 '[0] '[1])

 (def r2 (new-mol))
 (sync! r2 '[1] '[2 0])
 (newline) (displayln "0 = 1 = 2-0")
 (dm (pull r2 r1 '[]))
 )

(test-case
 "Inter-root advanced"
 (def model (new-mol))
 (sync! model '[0] '[1 0])

 (def r (new-mol))
 (update! r '[0] 'N)
 (pull! r model '[])
 (check-eq? (ref-data r '[1 0])
            'N))

(test-case
 "Advanced shit"
 (def sy sync)
 (def up update)
 (def ai (sy (up (up (new-mol) '[] 'ai=>) '[0] '->) '[0 0] '[0 1]))

 (def ak (sy (up (up (up (new-mol) '[] 'ak=>) '[0] '->) '[0 1] '->) '[0 0] '[0 1 1]))

 (def mp (sy (sy (up (up (new-mol) '[]  'mp=>) '[1 0]  '->) '[0]  '[1 0 1]) '[1 0 0]  '[2 0]))

 (newline) (displayln "Let's begin MOTHERFUCKER!")
 (def rt mp)
 (set! rt (pull rt ai '[1]))
 (displayln "Conclusion says (-> A (-> B A))")
 (dm (copy (pull rt ak '[2]) '[0]))

 (def rt2 mp)
 (set! rt2 (pull rt2 ak '[1]))
 (newline) (displayln "Conclusion says (-> A (-> B B))")
 (dm (copy (pull rt2 ai '[2]) '[0]))
 )
