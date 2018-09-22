#lang racket
(require "lang/kara.rkt"
         "mole.rkt")
(provide (all-defined-out))

(def up update)
(def sy sync)

(def ai
  ;; A -> A
  (sy (up (up (new-mol)
              '[] 'ai=>)
          '[0]    '->)

      '[0 0] '[0 1]))

(def ak
  ;; (-> A (-> B A))
  (sy (up (up (up (new-mol)
                  '[] 'ak=>)
              '[0]    '->)
          '[0 1]      '->)

      '[0 0] '[0 1 1]))

(def as
  ;; (=> (-> (-> A
  ;;             (-> B C))
  ;;         (-> (-> A B)
  ;;             (-> A C))))
  (let* ([r  (new-mol)]
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
  (sy (sy (up (up (new-mol)
                  '[]  'mp=>)
              '[1 0]  '->)

          '[0]  '[1 0 1]  #|A|#)
      '[1 0 0]  '[2 0]    #|B|#))

(def axioms (list ai ak as))
