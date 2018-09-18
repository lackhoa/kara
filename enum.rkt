#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt")

(provide (all-defined-out))

(def (complexity root)
  ;; Used to compare generality of two roots
  (let loop ([p null])
    (+ (match (eq? (ref-data root p)
                   'no-dat)
         [#t  0]
         [#f  1])
       (sub1 (length (ref-sync root p)))
       (sum-list (map loop
                      (kids-paths root p))))))

(def (replaceable? r1 r2)
  ;; Check if r1 is replaceable by r2
  (match (pull r1 r2)
    ['conflict  #f]
    [r1-pulled  (= (complexity r1)
                   (complexity r1-pulled))]))

(def (enum old new)
  ;; Return a list of molecules created from `old` and `new`
  (def (make-mp fun arg)
    (let* ([res (pull mp  fun '[1])]
           [res (pull res arg '[2])])
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

(def (cleanup old new)
  ;; Clean up old + new, returns the remaining bases
  ;; Remember: old molecules can still be contested
  (def conclusion
    (let ([ht  (hasheq)])
      (lam (m)
        (match (hash-ref ht m 'not-found)
          ['not-found  (let ([res  (detach m '[0])])
                         (set! ht (hash-set ht m res))
                         res)]
          [val         val]))))

  (let ([mols  (sort (append old new)
                     (lam (x y) (< (complexity (conclusion x))
                                 (complexity (conclusion y)))))])
    (let loop ([contestants  (list (car mols))]
               [contested    (cdr mols)])
      (match contested
        [(list)          contestants]
        [(cons fst rst)  (match (let/ec replace?
                                  (for ([contestant contestants])
                                    (when (and (or (memq contestant new)
                                                (memq contested  new)
                                                #|If both are old then they're orthogonal|#)
                                             (replaceable? (conclusion fst)
                                                           (conclusion contestant)))
                                      (call-with-output-file "discarded"
                                        #:exists 'truncate
                                        (lam (out)
                                          (displayln (mol-repr (conclusion fst)) out)
                                          (displayln "Replaced by" out)
                                          (displayln (mol-repr (conclusion contestant)) out)
                                          (newline out)))

                                      (replace? #t)))
                                  #f)
                           [#t  (loop contestants            rst)]
                           [#f  (loop (cons fst contestants) rst)])]))))

(def (main [old null] [new axioms])
  (let* ([newer-raw  (enum old new)]
         [familiar   (append old new)]
         [new-pool   (cleanup familiar newer-raw)])
    (cons (set-intersect new-pool familiar)
          (set-intersect new-pool newer-raw))))
