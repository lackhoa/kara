#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "enum.rkt"
         "types.rkt")

(def (query thm)
  (let ([db  (call-with-input-file "db/data.rkt"
               (lam (in) (read in)))])
    (for/or ([m  db])
      (match (instance thm (copy m '[0]))
        [#t  (dm m)]
        [#f  #f]))))

(def w
  ;; (A -> (A -> B)) -> (A -> B)
  (let ([m  (new-mol)])
    (update! m '[]      '->)
    (update! m '[0]     '->)
    (update! m '[0 0]   '?A)
    (update! m '[0 1]   '->)
    (update! m '[0 1 0] '?A)
    (update! m '[0 1 1] '?B)
    (update! m '[1]     '->)
    (update! m '[1 0]   '?A)
    (update! m '[1 1]   '?B)
    m))

(def c
  ;; (A -> (B -> C)) -> (B -> (A -> C))
  (let ([m  (new-mol)])
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

(def i
  (update (update (update (new-mol) '[] '->) '[0] '?A) '[1] '?A))

(query c)
