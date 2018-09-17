#lang racket
(require "lang/kara.rkt"
         "mole.rkt")

(provide ai ak as)

(def up update)
(def sy sync)

(def ai
  ;; A -> A
  (sy (up (up (new-root)
              '[]
              'ai=>)
          '[0]
          '->)
      '[0 0]
      '[0 1]))

(def ak
  ;; (-> A (-> B A))
  (sy (up (up (up (new-root)
                  '[]
                  'ak=>)
              '[0]
              '->)
          '[0 1]
          '->)
      '[0 0]
      '[0 1 1]))

(def as
  ;; (=> (-> (-> A
  ;;             (-> B C))
  ;;         (-> (-> A B)
  ;;             (-> A C))))
  (let* ([r  (new-root)]
         [r  (up r '[] 'as=>)]
         [r  (up r '[0] '->)]
         [r  (up r '[0 0] '->)]
         [r  (up r '[0 0 1] '->)]
         [r  (up r '[0 1] '->)]
         [r  (up r '[0 1 0] '->)]
         [r  (up r '[0 1 1] '->)]

         [r  (sy r '[0 0 0] '[0 1 0 0])]
         [r  (sy r '[0 0 0] '[0 1 1 0])]
         [r  (sy r '[0 0 1 0] '[0 1 0 1])]
         [r  (sy r '[0 0 1 1] '[0 1 1 1])])
    r))
