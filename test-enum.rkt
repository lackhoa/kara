#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "types.rkt"
         "enum.rkt"
         rackunit)

;;; Assignment (NOT mutation)
(define-syntax-rule (update! iden more ...)
  (set! iden (update iden more ...)))

(define-syntax-rule (sync! iden more ...)
  (set! iden (sync iden more ...)))

(define-syntax-rule (pull! iden more ...)
  (set! iden (pull iden more ...)))


(test-case
 "Generality"
 (check-true (instance? new-root
                        new-root))
 (check-false (instance? new-root
                         (update new-root '[] '->)))
 (check-true (instance? (update new-root '[] '->)
                        (update new-root '[] '->)))
 (check-true (instance? (sync (update (update new-root '[0] '->)
                                      '[1] '->)
                              '[0] '[1])
                        (sync new-root '[0] '[1])))
 (let ([base (sync (sync new-root '[0] '[1]) '[2] '[3])])
   (check-false (instance? base
                           (sync base '[1] '[2])))
   (check-true (instance? (sync base '[1] '[2])
                          base))))

(test-case
 "Multi-leveled"
 (def a->a (detach ai '[0]))
 (def r
   ;; (-> (-> (-> A B) (-> A B))
   ;;     (-> (-> A B) (-> A B)))
   (let ([m  new-root])
     (update! m '[] '->) (update! m '[0] '->) (update! m '[1] '->)
     (update! m '[0 0] '->) (update! m '[0 1] '->) (update! m '[1 0] '->) (update! m '[1 1] '->)
     (sync! m '[0 0 0] '[0 1 0]) (sync! m '[0 1 0] '[1 0 0]) (sync! m '[1 0 0] '[1 1 0])
     (sync! m '[0 0 1] '[0 1 1]) (sync! m '[0 1 1] '[1 0 1]) (sync! m '[1 0 1] '[1 1 1])
     m))

 (check-true (instance? r a->a))
 (check-false     (instance? a->a r)))

(test-case
 "Generality Ultimate"
 (def r1
   ;; (-> A (-> B A))
   (detach ak '[0]))

 (def r2
   ;; (-> (-> A B) (-> C (-> A B)))
   (let ([update! update] [sync! sync])
     (sync! (sync! (update! (update! (update! (update! new-root '[] '->) '[0] '->) '[1] '->) '[1 1] '->) '[0 0] '[1 1 0]) '[0 1] '[1 1 1])))

 (check-true (instance? r2 r1))
 (check-false (instance? r1 r2))
 (check-true (< (complexity r1)
                (complexity r2))))
