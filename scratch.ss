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

(def candidates  #| [mols] |#
  (let ([res  (make-vector (can-lim) '())])
    (vector-set! res 0 maxims
                 #|axioms' size are less than 16|#)
    res))

(define (load!)
  (set! db (file->list (db-file)))

  (for ([i  (in-range (can-lim))])
    (vector-set! candidates
                 i
                 (file->list (string-append (can-file)
                                            (~a i))))))

(for ([i  (in-range (can-lim))])
  (vector-set! candidates
               i
               (file->list (string-append (can-file)
                                          (~a i)))))

(define save
  (lambda ()
    (with-output-to-file (db-file)
      (lambda () (write db))))


  (for ([i  (in-range (can-lim))])
    (with-output-to-file (string-append (can-file)
                                        (~a i))
      #:exists 'truncate
      (thunk (for ([c  (league i)])
               (write c))))))


(define (add-can! c)
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
