(import (chezscheme)
        (kara-lang main))
(load "types.ss")
(load "enum.ss")
(load "stream.ss")
(load "mol.ss")

;;; State variables


;;; Parameters (files are preferably strings)
(define DB
  (append (ls-proof '((and 0 1) 0 1))
          knowledge
          apply-axioms
          circuit
          list-axioms
          (parse-circuit ca49)))

(define QUERY
  '(map (hi * !) (bob luke) 0))

(define MAX-STEPS     50)
(define TRIM?         #f)
(define MAX-CCS-SIZE  #f)

;;; Auxiliary routines
(define constant?
  (lambda (mol)
    (mol-< mol
           (lambda _    #f)
           (lambda _    #t)
           (lambda (pr)
             (and (constant? (car pr))
                (constant? (cdr pr)))))))

(define trim
  (lambda (proof)
    `(=> ,(get-ccs proof)
        ,@(>> (get-assumed proof)
              (l> filter (;; Don't show things that are trivially true
                          lambda (ccs)
                           (or (not (list? ccs))
                              (case (car ccs)
                                [(=/= !mem atom)  (not (constant? ccs))]
                                [else           #t]))))
              strip-duplicates))))

(define proof-steps
  ;; Counts how many => signs there are
  (lambda (proof)
    (mol-< proof
           (lambda _ 0)  (lambda _ (error "proof-steps" "Not a proof" proof))
           (lambda _
             (fold-left + 1
                        (let ([prem  (get-prem proof)])
                          (if (not (pair? prem))  (list)
                              (map proof-steps prem))))))))

(define size
  (f> mol-<
      (lambda _ 1)  (lambda _ 1)
      (lambda (pr)
        (+ (size (car pr))
           (size (cdr pr))))))


;;; Main Routines
(define cycle?
  ;; Test whether or not we're encountering goal duplication
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
                               (lambda _  #f)
                               (lambda (pr)
                                 (;; Only this proof's premises will be
                                  ;; checked against this conclusion
                                  ormap (f> loop (cons ccs seen))
                                        pr))))))))))

(define get-assumed
  (lambda (proof)
    (mol-< (get-prem proof)
           (lambda _     (list))
           (lambda (c)   (if (eq? c 'assumed)  (list (get-ccs proof))
                        (list)))
           (lambda (pr)  (flatmap get-assumed pr)))))

(define complete-proof?
  (lambda (proof)
    (mol-< (get-prem proof)
           (lambda _     #f)
           (;; Either assumed, already proven or no premise (a priori)
            lambda _     #t)
           (lambda (pr)  (for-all complete-proof? pr)))))

(define ccs-proven?
  (lambda (ccs proof)
    (or (and (equal? ccs
                  (get-ccs proof))
          (complete-proof? proof))

       (let ([prems  (get-prem proof)])
         (and (list? prems)
            (exists (l> ccs-proven? ccs)
               prems))))))

(define all-ccs
  (lambda (proof)
    (cons (get-ccs proof)
          (mol-< (get-prem proof)
                 (lambda _     (list))
                 (lambda _     (list))
                 (lambda (pr)  (flatmap all-ccs pr))))))

(define assumable?
  (f> mol-<
      (;; variables
       lambda _     #f)
      (;; constants
       lambda _     #f)
      (;; pairs
       lambda (pr)  (case (car pr)
                 [=/=     (not (equal? (cadr pr)
                                   (caddr pr)))]
                 [!mem  (or (not (list? (caddr pr)))
                           (not (member (cadr pr)
                                      (caddr pr))))]
                 [atom  (atom? (cadr pr))]
                 [else  #f]))))

(define illegal?
  ;; Check if a proof is in an illegal state
  (lambda (proof)
    (let ([assumed  (>> (get-assumed proof)
                        strip-duplicates)])
      (or (;; No cycle
          cycle? proof)
         (;; Only assume interesting things
          exists (negate assumable?)
            assumed)
         (;; Good size
          if (not MAX-CCS-SIZE)  #f
             (> (size (get-ccs proof))
                MAX-CCS-SIZE))))))

(define main
  (lambda (proof lpath)
    ;; `lpath` points to the list of remaining premises
    (let ([path  `[;; points to the current premise
                   ,@lpath car]])
      (;; path invalid -> no more premise in list
       if (not (ref proof path))  (stream proof)
          (>> (delay
                (if (;; If already proven then just say so
                     ccs-proven? (get-ccs (ref proof path))
                                 proof)
                    (cons (up proof `[,@path cdr cdr] 'proven)
                          (stream))

                    (cons (;; Base case: Assume
                           up proof `[,@path cdr cdr] 'assumed)
                          (;; Recursive case: Substitute an axiom if not out of steps
                           cond [(> (proof-steps proof)
                                    MAX-STEPS)  (stream)]
                                [else
                                 (>> (s-map (l> up proof path)
                                            (apply stream DB))
                                     (l> s-filter identity)
                                     (l> s-flatmap
                                         (;; Enumerate down
                                          f> main `[;; Premises of this
                                                    ,@path cdr cdr])))]))))
              (;; checking
               l> s-filter (negate illegal?))
              (;; move on to the other premises
               l> s-flatmap (f> main `[,@lpath cdr])))))))

(define b
  ;; The main stream
  (s-flatmap (f> main '[;; Top-level premise list
                        cdr cdr])
             (apply stream
               (>> (map (;; unify query with the conclusion
                         f> up '[cdr car] QUERY)
                        DB)
                   (l> filter identity)))))

;;; Tracing Business


;;; Jobs
(do ([i 1 (+ i 1)])
    [(or (s-null? b)
        (> i 500))]
  (>> (let ([res  (s-car b)])
        (set! b (s-cdr b))
        res)
      (lambda (x)
        (pydisplay (>> x (if TRIM? trim identity) clean))
        (pydisplay (proof-steps x) "steps"))))
