#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "enum.rkt"
         "types.rkt")
(provide (all-defined-out))

(print-graph #t)

(def DB-FILE   "db/db")
(def CAN-FILE  "db/can")
(def VIEW-FILE "db/view.rkt")
(def CAN-LIM   10)

(def db '()  #| [cmols] |#)
(def candidates  #| [cmols] |#
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

(define (main!)
  (def (select!)
    (#|Loop until we can find a new value for `the-reactor`|#
     for/or ([_  (in-naturals)])
      (let ([can  (get-can!)])
        (if (findf (lam (m) (instance? can (decompress m)))
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

    (cons! (compress the-reactor)
           db  #|db grows|#))

  (begin (select!) (col!) (com!)))
