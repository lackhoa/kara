#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt"
         "enum.rkt")

(test-case
 "Generality"
 (check-eq? (instance? (new-root)
                       (new-root))
            #t)
 (check-eq? (instance? (new-root)
                       (update (new-root) '[] '->))
            #f)
 (check-eq? (instance? (update (new-root) '[] '->)
                       (update (new-root) '[] '->))
            #t)
 (check-eq? (instance? (sync (update (update (new-root) '[0] '->)
                                     '[1] '->)
                             '[0] '[1])
                       (sync (new-root) '[0] '[1]))
            #t)
 (let ([base (sync (sync (new-root) '[0] '[1]) '[2] '[3])])
   (check-eq? (instance? base
                         (sync base '[1] '[2]))
              #f)))

(test-case
 "Generality Ultimate"
 (def r1
   ;; (-> A (-> B A))
   (detach ak '[0]))

 (def r2
   ;; (-> (-> A B) (-> C (-> A B)))
   (let ([up update] [sync sy])
     (sy (sy (up (up (up (up (new-root) '[] '->) '[0] '->) '[1] '->) '[1 1] '->) '[0 0] '[1 1 0]) '[0 1] '[1 1 1])))

 (check-eq? (instance? r2 r1)
            #t)
 (check-eq? (instance? r1 r2)
            #f))
