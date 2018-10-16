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

(def the-reactor #f)  #|mol%|#

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

(def (can-nums)
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

(def (get-can!)
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

(def (add-can! c)
  (let ([q  (quotient (height c) 5)])
    (cond [(< q CAN-LIM)  (vector-set! candidates
                                       q
                                       (cons c (league q)))]
          [else           (error "A super-height has appeared"
                                 (height c))])))

(def pls
  (build-list CORES
              (lam (_)
                (place ch
                  (let loop ()
                    (match (place-channel-get ch)
                      [`(extra-instance? ,can ,og)
                       (place-channel-put ch
                                          (instance? can (decompress og)))]
                      [`(collide ,reactor ,ort)
                       (place-channel-put ch
                                          (collide reactor ort))]
                      [`(combine ,reactor ,ort)
                       (place-channel-put ch
                                          (combine reactor ort))])
                    (loop))))))

(define (main!)
  (def (select!)
    (#|Loop until we can find a reactor|#
     for/or ([_  (in-naturals)])
      (let ([can  (get-can!)])
        (let ([involved  (if (len<= db CORES  #|Booting occurence|#)
                             (take pls (length db))
                             pls)])
          (for ([p      involved]
                [db-it  (take-not-more db CORES)])
            (place-channel-put p `(extra-instance? ,can ,db-it)))

          (let test-loop ([db-deck  (list-tail-safe db CORES)  #|Exhausting db|#]
                          [running  involved])
            (def (make-test-handler p)
              (handle-evt p
                          (lam (m)
                            (match m
                              [#t  (#|Abort candidate, wait for everyone to return|#
                                    let wait-loop ([running#  (sub1 (length running)
                                                                    #|minus `p`|#)])
                                     (match running#
                                       [0  #f]
                                       [_  (apply sync running)
                                           (wait-loop (sub1 running#))]))]
                              [#f  (match db-deck
                                     ['()         (test-loop '() (remove p running))]
                                     [(cons m ms)
                                      (place-channel-put p `(extra-instance? ,can ,m))
                                      (test-loop ms running)])])))
              #|Defined here b/c it refers to `db-deck` and `running`|#)
            (match running
              ['()  (set! the-reactor can)
               #|All cores approved|#]
              [_    (apply sync
                      (map make-test-handler running))]))))))

  (def (col!)
    (#|db to be reduced to its subset|#
     set! db
      (flatten1 (for/list ([db-part  (split-evenly db CORES)]
                           [pl       pls])
                  (place-channel-put/get pl
                                         `(collide ,the-reactor ,db-part))))))

  (def (com!)
    #|The only phase where db and candidates grow|#
    (for ([c  (flatten1 (for/list ([db-part  (split-evenly db CORES)]
                                   [pl       pls])
                          (place-channel-put/get pl
                                                 `(combine ,the-reactor ,db-part))))])
      (add-can! c))

    (>> (make-p the-reactor the-reactor)
        add-can!  #|Cover the blind spot|#)

    (cons! (compress the-reactor)
           db  #|db grows|#))

  (begin (select!)
         (col!)
         (com!)
         (collect-garbage)))
