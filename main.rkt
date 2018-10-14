#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "enum.rkt"
         "types.rkt")
(provide (all-defined-out))

(print-graph #t)

(def CORES     2)
(def DB-FILE   "db/db")
(def CAN-FILE  "db/can")
(def VIEW-FILE "db/view.rkt")
(def CAN-LIM   10)


(def db '())
(def candidates
  (let ([res  (make-vector CAN-LIM)])
    (vector-set! res 0 axioms
                 #|axioms' height < 10|#)
    res))

(def (load)
  (set! db (file->list DB-FILE))

  (for ([i  (in-range CAN-LIM)])
    (vector-set! candidates
                 i
                 (file->list (string-append CAN-FILE
                                            (~a i))))))

(def (num)
  (length db))

(def (save)
  (call-with-output-file DB-FILE
    #:exists 'truncate
    (lam (out) (for ([m  db])
               (wm m out))))

  (for ([i  (in-range CAN-LIM)])
    (call-with-output-file (string-append CAN-FILE
                                          (~a i))
      #:exists 'truncate
      (lam (out) (for ([c  (vector-ref candidates i)])
                 (wm c out))))))

(def (view)
  (call-with-output-file VIEW-FILE
    #:exists 'truncate
    (lam (out) (for ([m  db])
               (dm m) (newline out)))))

(def (query thm)
  (>> (for/or ([m  db])
        (instance? thm (decompress m)))
      dm))

(def (get-reactor)
  (let loop ([i  0])
    (cond [(= i CAN-LIM)  (error "No new reactor!")]
          [else           (match (vector-ref candidates i)
                            ['()         (loop (add1 i))]
                            [(cons r _)
                             (vector-set! candidates
                                          i
                                          (cdr (vector-ref candidates i)))
                             r])])))

(def (add-candidate c)
  (let ([q  (quotient (height c) 10)])
    (cond [(< q CAN-LIM)  (vector-set! candidates
                                       q
                                       (cons c (vector-ref candidates q)))]
          [else           (error "There is ")])))

(def (select)
  (for/or ([i  (in-naturals)  #|Loop until we can find a reactor|#])
    (let ([reactor  (get-reactor)]
          [dbs      (split-evenly db CORES)]
          [pls      (for/list ([i  (in-range CORES)])
                      (dynamic-place "enum.rkt"
                                     'place-original?))])
      (for ([i  (in-range CORES)]
            [p  pls])
        (place-channel-put p `(,reactor
                               ,(list-ref dbs i))))

      (let loop ([count  0])
        (match count
          [(== CORES)  reactor  #|All cores approved|#]
          [_          (let ([res  (apply sync pls)])
                        (match res
                          [#f  (begin (for-each place-kill pls)
                                      #f)  #|abort all|#]
                          [#t  (loop (add1 i))  #|continue|#]))])))))

(def (col reactor)
  (let ([dbs  (split-evenly db CORES)]
        [pls  (for/list ([i  (in-range CORES)])
                (dynamic-place "enum.rkt"
                               'place-collide))])
    (for ([i  (in-range CORES)]
          [p  pls])
      (place-channel-put p `(,reactor
                             ,(list-ref dbs i))))

    (set! db
      (flatten (map place-channel-get pls)))))

(def (com reactor)
  (let ([dbs  (split-evenly db CORES)]
        [pls  (for/list ([i  (in-range CORES)])
                (dynamic-place "enum.rkt"
                               'place-collide))])
    (for ([i  (in-range CORES)]
          [p  pls])
      (place-channel-put p `(,reactor
                             ,(list-ref dbs i))))
    (for ([c  (flatten (map place-channel-get
                            pls))])
      (add-candidate c))))
