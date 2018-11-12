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

(define category
  (list (#|Equality meaning|#
         mk-proof '((= 0 1))
                  '(and (= (dom 0) (dom 1))
                      (= (codom 0) (codom 1))))

        (#|Equality meaning 2|#
         mk-proof '((= 0 2) (= 1 3))
                  '(= (compose 0 1) (compose 2 3)))

        (#|Identity|#
         mk-proof '((im 0))
                  '(and (= (compose 0 1) 0)
                      (= (compose 1 0) 0)))

        (#|Composition associativity|#
         mk-proof '()
                  '(= (compose 0 (compose 1 2))
                      (compose (compose 0 1) 2)))

        (#|Terminal object|#
         mk-proof '((term 2)
                    (= (codom 0) 2)
                    (= (codom 1) 2))
                  '(= 0 1))

        (#|There is a terminal object|#
         mk-proof '()
                  '(term (1)))

        (#|Isomorphism|#
         mk-proof '((isomorphic 0 1))
                  '(and (im (compose (iso 0 1)
                             (iso 1 0)))
                      (im (compose (iso 1 0)
                             (iso 0 1)))))

        (#|Isomorphism demand|#
         mk-proof '((im (compose 2 3))
                    (im (compose 3 2))
                    (= (dom 2) 0)
                    (= (dom 3) 1))
                  '(isomorphic 0 1))

        (#|Definition of elements|#
         mk-proof '((elem 0 2))
                  '(and (= (dom 0) (1))
                      (= (codom 0) 2)))

        (#|Definition of an empty object|#
         mk-proof '((elem 0 (empty)))
                  '(f))

        (#|There is an empty object|#
         mk-proof '()
                  '(empty))

        (#|There is an empty object|#
         mk-proof '()
                  '(empty))

        (#|Function|#
         mk-proof '((= (compose 0 (f))
                       (compose 1 (f))))
                  '(= 0 1))

        (#|Cartesian product|#
         mk-proof '((prod? 0 1 2 3 4))
                  '(and (= (compose 3 (f12 5 6)) 5)
                      (= (compose 4 (f12 5 6)) 6)))

        (#|Cartesian product, uniqueness|#
         mk-proof '((prod? 0 1 2 3 4)
                    (= (compose 3 7) 5)
                    (= (compose 4 7) 6))
                  '(= 7 (f12 5 6)))

        (#|Cartesian product exists|#
         mk-proof '()
                  '(prod? (prod 0 1 (pr1 0 1) (pr2 0 1))))))
