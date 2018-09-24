#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt"
         "enum.rkt")

(test-case
 "Generality"
 (check-not-false (instance (new-mol)
                            (new-mol)))
 (check-not-false (instance (new-mol)
                            (update (new-mol) '[] '->)))
 (check-not-false (instance (update (new-mol) '[] '->)
                            (update (new-mol) '[] '->)))
 (check-not-false (instance (sync (update (update (new-mol) '[0] '->)
                                          '[1] '->)
                                  '[0] '[1])
                            (sync (new-mol) '[0] '[1])))
 (let ([base (sync (sync (new-mol) '[0] '[1]) '[2] '[3])])
   (check-false (instance base
                          (sync base '[1] '[2])))
   (check-not-false (instance (sync base '[1] '[2])
                              base))))

(test-case
 "Multi-leveled"
 (def a->a (copy ai '[0]))
 (def r
   ;; (-> (-> (-> A B) (-> A B))
   ;;     (-> (-> A B) (-> A B)))
   (let ([up update!] [sy sync!])
     (let ([m  (new-mol)])
       (up m '[] '->) (up m '[0] '->) (up m '[1] '->)
       (up m '[0 0] '->) (up m '[0 1] '->) (up m '[1 0] '->) (up m '[1 1] '->)
       (sy m '[0 0 0] '[0 1 0]) (sy m '[0 1 0] '[1 0 0]) (sy m '[1 0 0] '[1 1 0])
       (sy m '[0 0 1] '[0 1 1]) (sy m '[0 1 1] '[1 0 1]) (sy m '[1 0 1] '[1 1 1])
       m)))

 (check-not-false (instance r a->a))
 (check-false     (instance a->a r)))

(test-case
 "Generality Ultimate"
 (def r1
   ;; (-> A (-> B A))
   (copy ak '[0]))

 (def r2
   ;; (-> (-> A B) (-> C (-> A B)))
   (let ([up update] [sy sync])
     (sy (sy (up (up (up (up (new-mol) '[] '->) '[0] '->) '[1] '->) '[1 1] '->) '[0 0] '[1 1 0]) '[0 1] '[1 1 1])))

 (check-not-false (instance r2 r1))
 (check-false (instance r1 r2)))
