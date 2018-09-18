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

(def (enum combined uncombined)
  ;; Return a list of molecules created from `combined` and `uncombined`
  (def (make-mp fun arg)
    (let* ([res (pull mp  fun '[1])]
           [res (pull res arg '[2])])
      res))

  (let ([new null])
    ;; (c + u)^2 - c^2 = u^2 + c*u + u*c
    (for* ([rc  combined] [ru  uncombined])
      ;; Cartesian product
      (cons! (make-mp rc ru) new)
      (cons! (make-mp ru rc) new))

    (for* ([u1  uncombined] [u2  uncombined])
      (cons! (make-mp u1 u2) new))

    new))

(def (cleanup basis new)
  ;; Clean up old basis + new, returns the remaining basis
  ;; Remember: old molecules can still be contested
  (def-mem (conclusion m)
    (detach m '[0]))

  (call-with-output-file "discarded"
    #:exists 'truncate
    (lam (out)
      (void)))

  (let ([mols  (sort (append basis new)
                     (lam (x y) (or (< (complexity (conclusion x))
                                    (complexity (conclusion y)))
                                 (< (complexity x)
                                    (complexity y)))))])

    (let loop ([contestants  (list (car mols))]
               [contested    (cdr mols)])
      (match contested
        [(list)          contestants]
        [(cons fst rst)  (match (let/ec replace?
                                  (for ([contestant contestants])
                                    (when (and (or (memq contestant new)
                                                (memq fst        new)
                                                #|If both are old then they're orthogonal|#)
                                             (replaceable? (conclusion fst)
                                                           (conclusion contestant)))

                                      (call-with-output-file "discarded"
                                        #:exists 'append
                                        (lam (out)
                                          (displayln (mol-repr (conclusion fst)) out)
                                          (displayln "<=" out)
                                          (displayln (mol-repr (conclusion contestant)) out)
                                          (newline out)))

                                      (replace? #t)))
                                  #f)
                           [#t  (loop contestants            rst)]
                           [#f  (loop (cons fst contestants) rst)])]))))

(def (main [combined null]
           [uncombined axioms])
  (let* ([basis      (append combined uncombined)]
         [new        (enum combined uncombined)]
         [new-pool   (cleanup basis new)])
    (cons (set-intersect new-pool basis)
          (set-intersect new-pool new))))
