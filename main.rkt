#lang racket
(require "lang/kara.rkt"
         "ctr.rkt"
         "mol.rkt"
         "enum.rkt"
         "types.rkt")

(define (collide reactor ort)
  ;; mol% -> [mol%] -> [mol%]
  ;; Returns ort, after colliding with reactor
  (for/fold
      ([new-ort  '()])
      ([orti     ort])
    (if (instance? orti reactor)
        new-ort
        (cons orti new-ort))))

(define (make-p fun arg)
  ;; mol% -> mol% -> mol%
  (>> (up p '[0] fun)
      (f> up '[1] arg)
      (f> ref '[2]  #|get conclusion|#)))

(define (combine reactor ort)
  ;; mol% -> [mol%] -> [mol%]  (new formulas)
  (for/fold
      ([accu  '()])
      ([orti  ort])
    `(,@(remq #f `(,(make-p orti reactor)
                   ,(make-p reactor orti)))
      ,@accu)))

(define (split-evenly ls n)
  (let-values ([(q r)  (quotient/remainder (length ls)
                                           n)])
    (let loop ([ls   ls]
               [res  '()]
               [i    0])
      (cond [(= i (sub1 n))  (rcons res ls)]
            [else
             (let-values ([(ls1 ls2)  (split-at ls (if (< i r)
                                                       (add1 q)
                                                       q))])
               (loop ls2
                     (rcons res ls1)
                     (add1 i)))]))))

;;; Parameters
(debug? #f)
(def db-file   (make-parameter "db/data"))
(def can-file  (make-parameter "db/can"))
(def view-file (make-parameter "log/view.rkt"))
(def can-lim   (make-parameter 10))

;;; State
(def db '()      #| [mols] |#)
(def candidates  #| [mols] |#
  (let ([res  (make-vector (can-lim) '())])
    (vector-set! res 0 maxims
                 #|axioms' size are less than 16|#)
    res))

(def the-reactor #f)  #|mol%|#

;;; Procedures
(def (load!)
  (set! db (file->list (db-file)))

  (for ([i  (in-range (can-lim))])
    (vector-set! candidates
                 i
                 (file->list (string-append (can-file)
                                            (~a i))))))

(def (league i)
  (vector-ref candidates i))

(def (num)
  (length db))

(def (can-nums)
  (for/list ([i  (in-range (can-lim))])
    (length (league i))))

(def (save)
  (with-output-to-file (db-file)
    #:exists 'truncate
    (thunk (for ([m  db])
             (write m))))

  (for ([i  (in-range (can-lim))])
    (with-output-to-file (string-append (can-file)
                                        (~a i))
      #:exists 'truncate
      (thunk (for ([c  (league i)])
               (write c))))))

(def (view)
  (with-output-to-file (view-file)
    #:exists 'truncate
    (thunk (for ([m  db])
             (pdisplay m 35)
             (newline)))))

(def (query thm)
  (>> (findf (lam (m)
               (instance? thm m))
             db)
      pdisplay))

(def (get-can!)
  ;; -> mol%
  (let loop ([i  0])
    (cond [(= i (can-lim))  (error "No new reactor!")]
          [else             (match (league i)
                              ['()         (loop (add1 i))]
                              [(cons r _)
                               (vector-set! candidates
                                            i
                                            (cdr (league i)))
                               r])])))

(def (add-can! c)
  (let ([q  (- (exact-round (log (size c) 2)) 4
               #|less than 0 is less than a maxim|#)])
    (cond [(negative? q)  (vector-set! candidates
                                       0
                                       (cons c (league 0)))]
          [(< q (can-lim))  (vector-set! candidates
                                         q
                                         (cons c (league q)))]
          [else           (error "A super-height has appeared"
                                 c)])))

(define (main!)
  (def (select!)
    (#|Loop until we can find a new value for `the-reactor`|#
     for/or ([_  (in-naturals)])
      (let ([can  (get-can!)])
        (if (findf (lam (m) (instance? can m))
                   db)
            #f
            (set! the-reactor can)))))

  (def (col!)
    (set! db (collide the-reactor db)))

  (def (com!)
    #|The only phase where db and candidates grow|#
    (for ([c  (combine the-reactor db)])
      (add-can! c))

    (>> (make-p the-reactor the-reactor)
        add-can!  #|Cover the blind spot|#)

    (cons! the-reactor db  #|db grows|#))

  (begin (select!) (col!) (com!)))


;;; Jobs
;; (load!)
;; (repeat 100
;;         (thunk
;;          (repeat 20 (thunk
;;                      (main!)
;;                      (displayln (num))
;;                      (displayln (can-nums))))
;;          (save)))
