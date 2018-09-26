(def (instance? ins model)
  ;; Returns true iff ins is an instance of model.
  (def (same? root p1 p2)
    (or (member p1 (ref-sync root p2))
       (let ([ctor  (ref-data root p1)])
         (match (ref-data root p2)
           ['no-dat  #f]
           [ctor     (and (eq? (length (ref-kids root p1))
                             (hash-ref ctors ctor))
                        (eq? (length (ref-kids root p2))
                             (hash-ref ctors ctor))
                        (for/and ([kp1  (kids-paths root p1)]
                                  [kp2  (kids-paths root p2)])
                          (same? root kp1 kp2)))]
           [_        #f]))))

  (let loop ([path null])
    (and (or (eq? (ref-data model path)
               'no-dat)
          (eq? (ref-data ins path)
               (ref-data model path)))

       (for/and ([p  (ref-sync model path)])
         (same? ins path p))

       (<= (length (ref-kids model path))
          (length (ref-kids ins   path)))

       (andmap (lam (kid-path)  (loop kid-path))
               (kids-paths model path)))))

(def (main database)
  (def (make-mp fun arg)
    (pull (pull mp fun '[1]) arg '[2]))

  (def (conclusion root)
    (detach root '[0]))

  (def (log-discard m1 m2)
    (let ([mr1  (mol-repr (detach m1 '[0]))]
          [mr2  (mol-repr (detach m2 '[0]))])
      (unless (equal? mr1 mr2)
        (newline) (displayln mr1)
        (displayln "Replaced by")
        (displayln mr2) (newline))
      (display "x")))

  (let ([mixed  (shuffle database)]
        [i      -1])
    (let loop ([new-db null]
               [m1     (car mixed)]
               [mixed  (cdr mixed)])
      (set! i (add1 i))
      (match mixed
        [(list)          (cons m1 new-db)]
        [(cons m2 mrst)
         (match (< i 2)
           [#t  (match (make-mp m1 m2)
                  ['conflict  (loop (cons m1 new-db)
                                    m2
                                    mrst)]
                  [m3         (match (> (height (conclusion m3))
                                        10)
                                [#t  (loop (cons m1 new-db)  m2  mrst)  #|Gotta do w/o this one|#]
                                [#f  (loop (cons (pull (update (new-mol) '[] 'mp=>)
                                                       (conclusion m3)
                                                       '[0] #|We cannot store the entire proof|#)
                                                 (cons m1 new-db))
                                           m2
                                           mrst)])])]

           [#f  (match (instance? (conclusion m1)
                                  (conclusion m2))
                  [#t  (log-discard m1 m2)
                       (loop new-db  m2  mrst)]
                  [#f  (match (instance? m2 m1)
                         [#t  (log-discard m2 m1)
                              (loop new-db  m1  mrst)]
                         [#f  (loop (cons m1 new-db)
                                    m2
                                    mrst)])])])]))))
(pull (update (new-mol) '[] 'mp=>)
      (conclusion m3)
      '[0] #|We cannot store the entire proof |#)
