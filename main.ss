(import (chezscheme)
        (kara-lang main))
(load "types.ss")
(load "enum.ss")
(load "stream.ss")
(load "mol.ss")

;;; State variables


;;; Parameters (files are preferably strings)
(define DB
  (>> (append anti-unify
              lgg
              misc
              '((one) (two)))
      (l> apply ls-proof)))

(define QUERY
  ;; '(anti-unify ((reverse [tw o] [th] [o tw th]) :- (reverse [o] [tw th] [o tw th]))
  ;;              ((reverse [a] [] [a])            :- (reverse [] [a] [a]))
  ;;              T () S)
  '(lgg ((element c [b c])   :- (element c [c]))
        ((element d [b c d]) :- (element d [c d]) (element d [d]))
        C)
  )

(define MAX-STEPS     (make-parameter #f))
(define MAX-CCS-SIZE  (make-parameter #f))
(define TRIM?         #t)

;;; Auxiliary routines
(define constant?
  (lambda (mol)
    (mol-< mol
           (lambda _    #f)
           (lambda _    #t)
           (lambda (pr)
             (and (constant? (car pr))
                (constant? (cdr pr)))))))

(define fup
  ;; Force an update
  (lambda (mol path new)
    (cond [(null? path)  new]
          [else          (let ([pnext  (car path)]
                               [prest  (cdr path)])
                           (case pnext
                             [car  (cons (fup (car mol) prest new)
                                         (cdr mol))]
                             [cdr  (cons (car mol)
                                         (fup (cdr mol) prest new))]))])))

(define trim
  (lambda (proof)
    `(=> ,(get-ccs proof)
        ,@(>> (get-assumed-ccs proof)
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
  ;; Get the paths of assumed proofs
  (lambda (proof)
    (let loop ([;; Points to the current premise list
                lpath  #f])
      ;; Returns: lists of paths
      (let* ([path  (if (not lpath)  '[]
                        `[,@lpath car])]
             [;; The focused proof
              p     (ref proof path)])
        (if (not p)  (list)
            (append (mol-< (get-prem p)
                           (lambda _     (list))
                           (lambda (c)   (if (not (eq? c 'assumed))  (list)
                                        (list path)))
                           (lambda (pr)  (;; Go down to search its premises
                                     loop `[,@path cdr cdr])))
                    (if (not lpath)  '()  ;; We're done here
                        (loop `[;; Continue to the next sibling premises
                                ,@lpath cdr]))))))))

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

(define get-assumed-ccs
  ;; Get assumed conclusions from the list of paths
  (lambda (proof)
    (map (lambda (x)
           (>> x (l> ref proof) get-ccs))
         (get-assumed proof))))

(define illegal?
  (lambda (proof)
    (or (;; No cycle
         cycle? proof)
        (;; Good size
         if (not (MAX-CCS-SIZE))  #f
            (> (size (get-ccs proof))
               (MAX-CCS-SIZE))))))

(define reinforce
  ;; Check if a proof is in an illegal state, and transform it if needed
  ;; Returns: the (transformed) proof
  (lambda (proof)
    (and (not (illegal? proof))
       (;; Deal with assumptions
        let* ([assumed  (get-assumed proof)])
         (let loop ([proof    proof]
                    [assumed  assumed])
           (if (null? assumed)  proof
               (let* ([path   (car assumed)]
                      [ccs    (>> (ref proof path)
                                  get-ccs)])
                 (>> (mol-< ccs
                            (lambda (v)  #f) (lambda (c)  #f)
                            (;; pairs
                             lambda (pr)
                              (define ie-proc
                                ;; Change a proof to "immediately evident"
                                (f> fup `[,@path cdr cdr] '()))

                              (define ie (ie-proc proof))

                              (case (car ccs)
                                ;; Constraint
                                [=/=      (and (not (equal? (cadr ccs)
                                                      (caddr ccs)))
                                           (if (constant? ccs) ie
                                               proof))]
                                [!mem   (and (or (not (list? (caddr ccs)))
                                              (not (member (cadr ccs)
                                                         (caddr ccs))))
                                           (if (constant? ccs) ie
                                               proof))]
                                [atom   (and (atom? (cadr ccs))
                                           (if (constant? (cadr ccs)) ie
                                               proof))]
                                [;; Check the operator
                                 !^     (and (let ([op   (cadr ccs)]
                                                 [exp  (caddr ccs)])
                                             (or (and (var? exp) proof)
                                                (and (atom? exp) ie)
                                                (and (var? (car exp)) proof)
                                                (and (eq? (car exp) op) ie))))]

                                ;; Prolog Negation
                                [/+     (and (>> (parameterize ([MAX-STEPS #f])
                                                 (entry (cadr ccs)))
                                               s-null?)
                                           ie)]

                                ;; One-time computations
                                [var    (and (var? (cadr ccs)) ie)]
                                [!var   (and (not (var? (cadr ccs))) ie)]
                                [==      (and (equal? (cadr ccs)
                                                   (caddr ccs))
                                           ie)]
                                [unify  (>> (sync proof
                                                  `[,@path cdr car cdr car]
                                                  `[,@path cdr car cdr cdr car])
                                            ie-proc)]
                                [else   #f])))
                     (f> loop (cdr assumed))))))))))

(define main
  (lambda (proof lpath)
    ;; `lpath` points to the list of remaining premises
    (let ([;; points to the current premise
           path  (if (;;Starting up
                      not lpath)       '[]
                      `[,@lpath car])])
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
                           cond [(and (MAX-STEPS)
                                    (> (proof-steps proof)
                                       (MAX-STEPS)))
                                 (stream)]
                                [else
                                 (>> (s-map (l> up proof path)
                                            (apply stream DB))
                                     (l> s-filter identity)
                                     (l> s-flatmap
                                         (;; Enumerate down
                                          f> main `[;; Premises of this
                                                    ,@path cdr cdr])))]))))
              (;; checking
               l> s-map reinforce)
              (;; remove illegal states
               l> s-filter identity)
              (lambda (res-stream)
                (;; move on to the other premises, but if at the top level then return
                 if (not lpath)  res-stream
                    (s-flatmap (f> main `[,@lpath cdr])
                               res-stream))))))))

(define entry
  (lambda (query)
    (main (up '(=> 0 . 1) '[cdr car] query) #f)))

(define b
  ;; The main stream
  (entry (parse QUERY)))

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
