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

(def (enum mixed unmixed)
  ;; Return a list of molecules created by mixing up
  ;; `mixed` and `unmixed`
  (def (make-mp fun arg)
    (pull (pull mp fun '[1]) arg '[2]))

  (let* ([new          null]
         [maybe-cons!  (lam (x) (unless (eq? x 'conflict)
                                (cons! x new)))])

    ;; (A + B)^2 - B^2 = A^2 + A*B + B*A
    (for* ([r1  mixed]
           [r2  unmixed])
      ;; Cartesian product
      (maybe-cons! (make-mp r1 r2))
      (maybe-cons! (make-mp r2 r1)))

    (for* ([r1  unmixed]
           [r2  unmixed])
      (maybe-cons! (make-mp r1 r2)))

    new))

(def (cleanup cleaned new)
  ;; Clean up cleaned + new, returns the remaining pool
  ;; Note: The old cleaned pool can still be contested
  (def-mem (conclusion m)
    (detach m '[0]))

  (call-with-output-file "discarded"
    #:exists 'truncate
    (lam (out) (void)))

  (let ([mols  (sort (append cleaned new)
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

(def (main [l1 null] [l2 axioms])
  ;; l1 is clean and mixed
  ;; l2 is clean and unmixed
  ;; => old+new: clean and unmixed
  ;; Returns: two lists with the same characteristics.
  (let* ([l3     (enum l1 l2)]
         [l12    (append l1 l2)]
         [cl123  (cleanup l12 l3)])
    (cons (set-intersect cl123 l12)
          (set-intersect cl123 l3))))
