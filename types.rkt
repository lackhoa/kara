#lang racket
(require "lang/kara.rkt"
         "mol.rkt")
(provide (all-defined-out))

(def up update)
(def sy msync)

;;; Axioms
(def i
  ;; A -> A
  (sy (up new-root '[] '->)
      '[0] '[1]))

(def k
  ;; (-> A (-> B A))
  (sy (up (up new-root '[] '->)
          '[1] '->)
      '[0] '[1 1]))

(def s
  ;; (-> (-> A
  ;;         (-> B C))
  ;;     (-> (-> A B)
  ;;         (-> A C)))
  (>> new-root
      (lam (r)  (up r '[]    '->))
      (lam (r)  (up r '[0]   '->))
      (lam (r)  (up r '[0 1] '->))
      (lam (r)  (up r '[1]   '->))
      (lam (r)  (up r '[1 0] '->))
      (lam (r)  (up r '[1 1] '->))
      (lam (r)  (sy r '[0 0]   '[1 0 0]))
      (lam (r)  (sy r '[0 0]   '[1 1 0]))
      (lam (r)  (sy r '[0 1 0] '[1 0 1]))
      (lam (r)  (sy r '[0 1 1] '[1 1 1]))))

(def ai
  (>> (up new-root '[] 'ai=>)
      (lam (r)  (pull r '[0] i))))

(def as
  (>> (up new-root '[] 'as=>)
      (lam (r)  (pull r '[0] s))))

(def ak
  (>> (up new-root '[] 'ak=>)
      (lam (r)  (pull r '[0] k))))

(def axioms `(,ak ,as))
(def short-axioms  `(,k ,s))

(def p
  ;; (B (-> A B) A)
  (sy (sy (up new-root '[1] '->)
          '[0] '[1 1]  #|B|#)
      '[1 0] '[2]      #|A|#))

(def mp
  ;; (mp=> B
  ;;       (=> (-> A B))
  ;;       (=> A))
  (sy (sy (up (up new-root '[] 'mp=>)
              '[1 0] '->)
          '[0] '[1 0 1]  #|B|#)
      '[1 0 0] '[2 0]    #|A|#))

;;; Some theorems to prove
(def w
  ;; (A -> (A -> B)) -> (A -> B)
  (>> new-root
      (lam (m) (up m '[]      '->))
      (lam (m) (up m '[0]     '->))
      (lam (m) (up m '[0 0]   'A))
      (lam (m) (up m '[0 1]   '->))
      (lam (m) (up m '[0 1 0] 'A))
      (lam (m) (up m '[0 1 1] 'B))
      (lam (m) (up m '[1]     '->))
      (lam (m) (up m '[1 0]   'A))
      (lam (m) (up m '[1 1]   'B))))

(def c
  ;; (A -> (B -> C)) -> (B -> (A -> C))
  (>> new-root
      (lam (m) (up m '[]      '->))
      (lam (m) (up m '[0]     '->))
      (lam (m) (up m '[0 0]   'A))
      (lam (m) (up m '[0 1]   '->))
      (lam (m) (up m '[0 1 0] 'B))
      (lam (m) (up m '[0 1 1] 'C))
      (lam (m) (up m '[1]     '->))
      (lam (m) (up m '[1 0]   'B))
      (lam (m) (up m '[1 1]   '->))
      (lam (m) (up m '[1 1 0] 'A))
      (lam (m) (up m '[1 1 1] 'C))))

(def b
  ;; (B -> C) -> ((A -> B) -> (A -> C))
  (>> new-root
      (lam (m)  (up m '[]      '->))
      (lam (m)  (up m '[0]     '->))
      (lam (m)  (up m '[1]     '->))
      (lam (m)  (up m '[1 0]   '->))
      (lam (m)  (up m '[1 1]   '->))
      (lam (m)  (up m '[1 0 0] 'A))
      (lam (m)  (up m '[1 1 0] 'A))
      (lam (m)  (up m '[0 0]   'B))
      (lam (m)  (up m '[1 0 1] 'B))
      (lam (m)  (up m '[0 1]   'C))
      (lam (m)  (up m '[1 1 1] 'C))))
