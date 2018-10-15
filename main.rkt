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


(def db '()  #|[cmols]|#)
(def candidates
  #|[cmols]|#
  (let ([res  (make-vector CAN-LIM '())])
    (vector-set! res 0 (map compress short-axioms)
                 #|axioms' height < 10|#)
    res))
(def the-reactor #f)

(def (load!)
  (set! db (file->list DB-FILE))

  (for ([i  (in-range CAN-LIM)])
    (vector-set! candidates
                 i
                 (file->list (string-append CAN-FILE
                                            (~a i))))))

(def (league i)
  (vector-ref candidates i))

(def (num)
  (length db))

(def (can-num)
  (for/list ([i  (in-range CAN-LIM)])
    (length (league i))))

(def (save)
  (call-with-output-file DB-FILE
    #:exists 'truncate
    (lam (out) (for ([m  db])
               (wm m out))))

  (for ([i  (in-range CAN-LIM)])
    (call-with-output-file (string-append CAN-FILE
                                          (~a i))
      #:exists 'truncate
      (lam (out) (for ([c  (league i)])
                 (wm c out))))))

(def (view)
  (call-with-output-file VIEW-FILE
    #:exists 'truncate
    (lam (out) (for ([m  db])
               (dm m out) (newline out)))))

(def (query thm)
  (>> (findf (lam (m)
               (instance? thm (decompress m)))
             db)
      dm))

(def (get-reactor)
  ;; -> mol%
  (let loop ([i  0])
    (cond [(= i CAN-LIM)  (error "No new reactor!")]
          [else           (match (league i)
                            ['()         (loop (add1 i))]
                            [(cons r _)
                             (vector-set! candidates
                                          i
                                          (cdr (league i)))
                             (decompress r)])])))

(def (add-candidate c)
  (let ([q  (quotient (height c) 5)])
    (cond [(< q CAN-LIM)  (vector-set! candidates
                                       q
                                       (cons c (league q)))]
          [else           (error "There is ")])))

(def (select!)
  (#|Loop until we can find a reactor|#
   for/or ([_  (in-naturals)])
    (let ([reactor  (get-reactor)]
          [dbs      (split-evenly db CORES)]
          [pls      (build-list CORES
                                (lam (_)
                                  (dynamic-place "enum.rkt"
                                                 'place-original?)))])
      (for ([i  (in-range CORES)]
            [p  pls])
        (place-channel-put p `(,reactor
                               ,(list-ref dbs i))))

      (let loop ([count  0])
        (match count
          [(== CORES)  (set! the-reactor reactor)
           #|All cores approved|#]
          [_          (let ([res  (apply sync pls)])
                        (match res
                          [#f  (begin (for-each place-kill pls)
                                      #f)  #|abort all|#]
                          [#t  (loop (add1 count))
                               #|continue|#]))])))))

(def (col!)
  (let ([dbs  (split-evenly db CORES)]
        [pls  (build-list CORES
                          (lam (_)
                            (dynamic-place "enum.rkt"
                                           'place-collide)))])
    (for ([i  (in-range CORES)]
          [p  pls])
      (place-channel-put p `(,the-reactor
                             ,(list-ref dbs i))))

    (set! db
      (flatmap place-channel-get pls)
      #|db to contain what's left|#)))

(def (com!)
  #|The only phase where db and candidates grow|#
  (let ([dbs  (split-evenly db CORES)]
        [pls  (build-list CORES
                          (lam (_)
                            (dynamic-place "enum.rkt"
                                           'place-combine)))])
    (for ([i  (in-range CORES)]
          [p  pls])
      (place-channel-put p `(,the-reactor
                             ,(list-ref dbs i))))

    (for ([c  (flatmap place-channel-get
                       pls)])
      (add-candidate c))

    (>> (make-p the-reactor the-reactor)
        add-candidate  #|Cover the blind spot|#)

    (cons! (compress the-reactor)
           db  #|db grows|#)))


;;; Jobs
(def (job1)
  (load!)
  (repeat 5
          (thunk (select!)
                 (col!)
                 (com!)
                 (displayln (num))
                 (displayln (can-num))))
  (save))

;; (job1)
