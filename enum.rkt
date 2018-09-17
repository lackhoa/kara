#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt")

(provide (all-defined-out))

(def (complexity root [path null])
  ;; Used to compare generality.
  (let loop ([p path])
    (+ (match (eq? (ref-data root p)
                   'no-dat)
         [#t  0]
         [#f  1])
       (sub1 (length (filter (curry list-prefix? path)
                             (ref-sync root p))))
       (sum-list (map loop
                      (kids-paths root p))))))

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

(def (cleanup mols)
  (let loop ([i    1]
             [mols (sort mols (lam (x y) (< (complexity x '[0])
                                          (complexity y '[0]))))])
    (match (<= i (last-index mols))
      [#t  (let ([contested   (list-ref mols i)]
                 [contestants (take mols i)])
             (match (let/ec replace?
                      (for ([contestant contestants])
                        (when (replaceable? contested contestant '[0] '[0])
                          (replace? #t)))
                      #f)
               [#t  (loop i        (drop-pos mols i))]
               [#f  (loop (add1 i) mols)]))]
      [#f  mols])))
