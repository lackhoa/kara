#lang racket
(require "lang/kara.rkt"
         "mol.rkt")
(provide (all-defined-out))

(def up update)
(def sy sync)

;;; Axioms
(def ai
  ;; A -> A
  (sy (up (up new-root
              '[] 'ai=>)
          '[0]    '->)

      '[0 0] '[0 1]))

(def ak
  ;; (-> A (-> B A))
  (sy (up (up (up new-root
                  '[] 'ak=>)
              '[0]    '->)
          '[0 1]      '->)

      '[0 0] '[0 1 1]))

(def as
  ;; (=> (-> (-> A
  ;;             (-> B C))
  ;;         (-> (-> A B)
  ;;             (-> A C))))
  (let* ([r  new-root]
         [r  (up r '[]      'as=>)]
         [r  (up r '[0]     '->)]
         [r  (up r '[0 0]   '->)]
         [r  (up r '[0 0 1] '->)]
         [r  (up r '[0 1]   '->)]
         [r  (up r '[0 1 0] '->)]
         [r  (up r '[0 1 1] '->)]

         [r  (sy r '[0 0 0]   '[0 1 0 0])]
         [r  (sy r '[0 0 0]   '[0 1 1 0])]
         [r  (sy r '[0 0 1 0] '[0 1 0 1])]
         [r  (sy r '[0 0 1 1] '[0 1 1 1])])
    r))

(def mp
  ;; (=> A
  ;;     (=> (-> B A))
  ;;     (=> B))
  (sy (sy (up (up new-root
                  '[]  'mp=>)
              '[1 0]  '->)

          '[0]  '[1 0 1]  #|A|#)
      '[1 0 0]  '[2 0]    #|B|#))

(def axioms (list ai ak as))

;;; Some theorems to prove
(def w
  ;; (A -> (A -> B)) -> (A -> B)
  (let ([m  new-root])
    (update! m '[]      '->)
    (update! m '[0]     '->)
    (update! m '[0 0]   '?A)
    (update! m '[0 1]   '->)
    (update! m '[0 1 0] '?A)
    (update! m '[0 1 1] '?B)
    (update! m '[1]     '->)
    (update! m '[1 0]   '?A)
    (update! m '[1 1]   '?B)
    m)  #|Proven|#)

(def c
  ;; (A -> (B -> C)) -> (B -> (A -> C))
  (let ([m  new-root])
    (update! m '[]      '->)
    (update! m '[0]     '->)
    (update! m '[0 0]   '?A)
    (update! m '[0 1]   '->)
    (update! m '[0 1 0] '?B)
    (update! m '[0 1 1] '?C)
    (update! m '[1]     '->)
    (update! m '[1 0]   '?B)
    (update! m '[1 1]   '->)
    (update! m '[1 1 0] '?A)
    (update! m '[1 1 1] '?C)
    m))

(def i (detach ai '[0]))

(def k (detach ak '[0]))

(def s (detach as '[0]))
