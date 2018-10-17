#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "types.rkt"
         "enum.rkt"
         rackunit)

(def sy msync)

(def up update)

(define-syntax-rule (up! iden more ...)
  (set! iden (update iden more ...)))

(define-syntax-rule (sy! iden more ...)
  (set! iden (msync iden more ...)))

(define-syntax-rule (pull! iden more ...)
  (set! iden (pull iden more ...)))


(test-case
 "Trivial"
 (check-false (instance? (update new-root '[] 'a)
                         (update new-root '[] 'b)))
 (check-true (instance? new-root
                        new-root)))

(test-case
 "Generality"
 (check-false (instance? new-root
                         (update new-root '[] '->)))
 (check-true (instance? (update new-root '[] '->)
                        (update new-root '[] '->)))
 (check-true (instance? (msync (update (update new-root '[0] '->)
                                       '[1] '->)
                               '[0] '[1])
                        (msync new-root '[0] '[1])))
 (let ([base (msync (msync new-root '[0] '[1]) '[2] '[3])])
   (check-false (instance? base
                           (msync base '[1] '[2])))
   (check-true (instance? (msync base '[1] '[2])
                          base))))

(test-case
 "Multi-leveled"
 (def a->a i)
 (def r
   ;; (-> (-> (-> A B) (-> A B))
   ;;     (-> (-> A B) (-> A B)))
   (let ([m  new-root])
     (up! m '[] '->) (up! m '[0] '->) (up! m '[1] '->)
     (up! m '[0 0] '->) (up! m '[0 1] '->) (up! m '[1 0] '->) (up! m '[1 1] '->)
     (sy! m '[0 0 0] '[0 1 0]) (sy! m '[0 1 0] '[1 0 0]) (sy! m '[1 0 0] '[1 1 0])
     (sy! m '[0 0 1] '[0 1 1]) (sy! m '[0 1 1] '[1 0 1]) (sy! m '[1 0 1] '[1 1 1])
     m))

 (check-true (instance? r a->a))
 (check-false     (instance? a->a r)))

(test-case
 "Generality Ultimate"
 (def r1
   ;; (-> A (-> B A))
   (detach k '[0]))

 (def r2
   ;; (-> (-> A B) (-> C (-> A B)))
   (sy (sy (up (up (up (up new-root '[] '->) '[0] '->) '[1] '->) '[1 1] '->) '[0 0] '[1 1 0]) '[0 1] '[1 1 1]))

 (check-true (instance? r2 r1))
 (check-false (instance? r1 r2))
 (check-true (< (complexity r1)
                (complexity r2))))

(test-case
 "Super edgy cases (well not really)"
 (def r1
   ;; (-> (-> A B) (-> B A))
   (sy (sy (up (up (up new-root '[] '->) '[0] '->) '[1] '->) '[0 0] '[1 1]) '[0 1] '[1 0]))

 (def r2
   ;; (-> A A)
   (sy (up (up (up new-root '[] '->) '[0] '->) '[1] '->) '[0] '[1]))

 (check-false (instance? r2 r1))

 (def r3
   ;; (-> (-> A ?) (-> ? A))
   (sy (up (up (up new-root '[] '->) '[0] '->) '[1] '->) '[0 0] '[1 1]))

 (check-false (instance? r2 r3))
 (check-true (instance? r1 r3))
 )
