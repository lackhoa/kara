(import (chezscheme)
        (kara-lang main))
(load "types.ss")
(load "enum.ss")
(load "stream.ss")
(load "mol.ss")

;;; State variables


;;; Parameters (files are preferably strings)
(define db
  (append base equality substitution category))

(define boring
  (append (list-head equality 2)
          substitution))

(define max-steps    11)
(define trim?        #t)
(define show-num     500)

(define allowed-ctors
  '(= map prop))

;;; Main Routines
(define cycle?
  #|Test whether or not we're encountering goal duplication|#
  (lambda (proof)
    (let loop ([mol   proof]
               [seen  (list)])
      (mol-< mol
             (lambda _  #f)  (lambda _  #f)
             (lambda _  (or (bool (member (get-ccs mol)
                                    seen))
                      (ormap (f> loop (cons (get-ccs mol)
                                            seen))
                             (get-prem mol))))))))

(define proof-steps
  #|Counts how many => signs there are|#
  (lambda (proof)
    (mol-< proof
           (lambda _ 0)  (lambda _ (error "proof-steps" "Not a proof" proof))
           (lambda _
             (fold-left + 1 (map proof-steps (get-prem proof)))))))

(define get-ungrounded
  (lambda (proof)
    (define get-ungrounded-core
      (lambda (proof)
        (mol-< (ref proof '[cdr cdr]  #|Get premises list|#)
               (lambda _    (list (get-ccs proof)))
               (lambda (c)  (assert (null? c))  c)
               (lambda _    (flatmap get-ungrounded-core
                                (get-prem proof))))))

    (>> (get-ungrounded-core proof)
        strip-duplicates)))

(define trim
  (lambda (proof)
    `(=> ,(get-ccs proof)
        ,@(get-ungrounded proof))))

(define shuffle
  ;; Warning: sloppy style!
  (lambda (ls)
    (if (< (length ls) 2)  ls
        (let ([item  (list-ref ls (random (length ls)))])
          (cons item
                (shuffle (remove item ls)))))))

(define assumable?
  (f> mol-<
      (;; variables
       lambda _     #f)
      (;; constants
       lambda _     #f)
      (;; pairs
       lambda (pr)  (bool (;; of the form (allowed-ctor vars ...)
                      and (memq (car pr)  allowed-ctors)
                        (for-all number?      (cdr pr)))))))

(define main
  (lambda (proof lpath)
    ;; `lpath points to the list of remaining premises`
    (let ([path  `[;; points to the current premise
                   ,@lpath car]])
      (;; path invalid -> no more premise in list
       if (not (ref proof path))  (stream proof)
          (>> (s-append
               (;; Just assume it (base case)
                delay
                 (cond [(>> (ref proof path)
                            get-ccs  assumable?)
                        (force (stream proof))]
                       [else                    (force (stream))]))

               (;; Substitute an axiom (recursive case)
                delay
                 (force
                  (;; limit size for complete search
                   cond [(> (proof-steps proof)
                            max-steps)  (stream)]

                        [else  (;; enumerate down
                                >> (s-map (l> up proof path)
                                          (apply stream db))
                                   (l> s-filter
                                       (f>> (negate cycle?)))
                                   (l> s-flatmap
                                       (f> main `[#|premise of this proof|#
                                                  ,@path cdr cdr])))]))))
              (l> s-flatmap
                  (;; move on to the other premises
                   f> main `[,@lpath cdr])))))))

(define query
  '(im 0))

(define b
  ;; The main stream
  (s-flatmap (f> main '[cdr cdr  #|Top-level premise list|#])
             (apply stream
               (>> (filter (negate (f> member boring))
                           db)
                   (l> map (;; unify query with the conclusion
                            f> up '[cdr car] query))
                   (l> filter identity)))))

;;; Tracing Business


;;; Jobs
(do ([i 1 (+ i 1)])
    [(or (s-null? b)
        (> i show-num))]
  (>> (let ([res  (s-car b)])
        (set! b (s-cdr b))
        res)
      (lambda (x)
        (pydisplay (>> x (if trim? trim identity) clean))
        (pydisplay (proof-steps x) "steps"))))
