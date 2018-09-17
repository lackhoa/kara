#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt")

(provide (all-defined-out))

(def (complexity root [path null])
  ;; Used to compare generality.
  (+ (match (eq? (ref-data root path)
                 'no-dat)
       [#t  0]
       [#f  1])
     (sub1 (length (filter (curryr list-prefix? path)
                           (ref-sync root path))))
     (sum-list (map (curry complexity root)
                    (kids-paths root path)))))

(def (replaceable? r1 r2 p1 p2)
  ;; See if r1-p1 is replaceable by r2-p2
  (match (pull r1 r2 p1 p2)
    ['conflict  #f]
    [pulled     (= (complexity r1 p1)
                   (complexity pulled p1))]))

(def (enum old new)
  (def (make-mp fun arg)
    (let* ([res (pull mp  fun '[1] '[])]
           [res (pull res arg '[2] '[])])
      res))

  (let ([newer null])
    (for* ([ro  old] [rn  new])
      (cons! (make-mp ro rn)
             newer)
      (cons! (make-mp rn ro)
             newer))

    (for ([pair (in-combinations new 2)])
      (cons! (make-mp (first pair) (second pair))
             newer)
      (cons! (make-mp (second pair) (first pair))
             newer))

    newer))
