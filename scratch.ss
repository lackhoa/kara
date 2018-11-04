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

(define go
  (lambda ()
    (do ([res #f (ma)])
        [(>> res
             (lambda (x)
               (andmap (lambda (thm)
                         (not (instance? res thm)))
                       (append db blacklist))))
         (set! current res)
         res])))

(define ma
  (lambda ()
    (>> (let loop ([root  mp]
                   [path  '[]])
          (let ([candidates
                 (#|Check if there is an axiom waiting|#
                  >> (map (l> up root `[,@path 2])
                          db)
                     (l> filter (f>> (negate cycle?))))])
            (cond [(and (null? candidates)
                      (not (eq? (>> (ref root path)
                                  conclusion
                                  car)
                              '->)))
                   root]
                  [(and (not (null? candidates))
                      (not (equal? path '[]))
                      (not (ran-elem (dice))))
                   (>> (ran-elem candidates)
                       (f> up `[,@path 0] '(f))
                       (f> up `[,@path 1] '(f))
                       #|Knock off the hypotheses|#)]
                  [(< (size root) (size-limit))
                   (#|Keep grinding with modus ponens|#
                    >> (up root path mp)
                       (f> loop `[,@path 0])
                       (f> loop `[,@path 1]))]
                  [else  #f])))
        shorten
        clean)))

(define ma
  #|Returns a proof|#
  (lambda (dep counter)
    (cond [#|Variable|#(= counter 0)  new-var]
          [#|atomic|#  (< counter (+ db-len 1))
                       `(=> (f) (f)
                           ,(list-ref db (- counter 1)))]
          [#|application|#
           else  (let ([d-1  (- dep 1)])
                   (let-values ([(i0 i1)
                                 (div-and-mod (- counter db-len 1)
                                              (N d-1))])
                     (let* ([func  (ma d-1 i0)]
                            [arg   (and func (ma d-1 i1))])
                       (and arg
                          (>> (up mp '[0] func)
                              (f> up '[1] arg))))))])))
