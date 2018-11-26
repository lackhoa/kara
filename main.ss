(import (chezscheme)
        (kara-lang main))
(load "types.ss")
(load "enum.ss")
(load "stream.ss")
(load "mol.ss")

;;; State variables


;;; Parameters (files are preferably strings)
(define db
  (append substitution
          equality))

(define max-steps    10)
(define trim?        #t)
(define show-num     500)

(define allowed-ctors
  '(= map im type))

;;; Auxiliary routines
(define trim
  (lambda (proof)
    `(=> ,(get-ccs proof)
        ,@(get-assumed proof))))

(define shuffle
  ;; Warning: sloppy style!
  (lambda (ls)
    (if (< (length ls) 2)  ls
        (let ([item  (list-ref ls (random (length ls)))])
          (cons item
                (shuffle (remove item ls)))))))

(define proof-steps
  #|Counts how many => signs there are|#
  (lambda (proof)
    (mol-< proof
           (lambda _ 0)  (lambda _ (error "proof-steps" "Not a proof" proof))
           (lambda _
             (fold-left + 1
                        (let ([prem  (get-prem proof)])
                          (if (not (pair? prem))  (list)
                              (map proof-steps prem))))))))


;;; Main Routines
(define cycle?
  #|Test whether or not we're encountering goal duplication|#
  (lambda (proof)
    (let loop ([proof  proof]
               [seen   (list)])
      (mol-< proof
             (lambda _  #f)
             (lambda _  #f)
             (lambda _  (let ([ccs   (get-ccs proof)])
                     (or (bool (member ccs seen))
                        (mol-< (get-prem proof)
                               (lambda _  #f)
                               (lambda (c)
                                 (case c
                                   [(assumed ())  #f]
                                   [else          (error "cycle?" "What?" c)]))
                               (lambda (pr)
                                 (;; Only this proof's premises will be
                                  ;; checked against this conclusion
                                  ormap (f> loop (cons ccs seen))
                                        pr))))))))))

(define get-assumed
  (lambda (proof)
    (>> (let assumed-core ([proof  proof])
          (mol-< (get-prem proof)
                 (lambda _     (list))
                 (lambda (c)   (case c
                            ['()      (list)]
                            [assumed  (list (get-ccs proof))]
                            [else     (error "get-assumed" "What?")]))
                 (lambda (pr)  (flatmap assumed-core pr))))

        strip-duplicates)))

(define assumable?
  (f> mol-<
      (;; variables
       lambda _     #f)
      (;; constants
       lambda _     #f)
      (;; pairs
       lambda (pr)  (bool (;; of the form (allowed-ctor vars ...)
                      and (memq (car pr)  allowed-ctors)
                        (for-all number? (cdr pr)))))))

(define illegal?
  ;; Check if a proof is in an illegal state
  (lambda (proof)
    (or (cycle? proof)
       (exists (negate assumable?)
          (get-assumed proof)))))

(define main
  (lambda (proof lpath)
    ;; `lpath points to the list of remaining premises`
    (let ([path  `[;; points to the current premise
                   ,@lpath car]])
      (;; path invalid -> no more premise in list
       if (not (ref proof path))  (stream proof)
          (>> (delay
                (cons
                 #|Base case: Assume|#
                 (up proof  `[,@path cdr cdr]  'assumed)
                 (;; Recursive case: Substitute an axiom
                  delay
                   (force
                    (;; limit size for complete search
                     cond [(> (proof-steps proof)
                              max-steps)  (stream)]

                          [else  (;; enumerate down
                                  >> (s-map (l> up proof path)
                                            (apply stream db))
                                     (l> s-filter identity)
                                     (l> s-flatmap
                                         (f> main `[#|premises of this|#
                                                    ,@path cdr cdr])))])))))
              (;; checking
               l> s-filter  (negate illegal?))
              (;; move on to the other premises
               l> s-flatmap  (f> main `[,@lpath cdr])))))))

(define query
  '(= 0 1))

(define b
  ;; The main stream
  (s-flatmap (f> main '[cdr cdr  #|Top-level premise list|#])
             (apply stream
               (>> (map (;; unify query with the conclusion
                         f> up '[cdr car] query)
                        db)
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
