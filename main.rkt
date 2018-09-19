#! /usr/bin/racket
#lang racket

(require "lang/kara.rkt"
         "mole.rkt"
         "enum.rkt"
         "types.rkt")

(def (dm mol [port (current-output-port)])
  (pdisplay (mol-repr mol) 35 port))

(def (boot)
  (let ([result (main)])
    (call-with-output-file "db/data"
      ;; Initialize
      #:exists 'truncate
      (lam (out)
        (write result out)))
    (call-with-output-file "db/debug"
      #:exists 'truncate
      (lam (out)
        (for ([m (append (car result)
                         (cdr result))])
          (dm m out)
          (newline out))))))

(def (round1)
  (call-with-input-file "db/data"
    (lam (in)
      (match (read in)
        [(cons mixed unmixed)
         (let ([result (main mixed unmixed)])
           ;; Write the db/data
           (call-with-output-file "db/data1"
             #:exists 'truncate
             (lam (out)
               (write result out)))

           (call-with-output-file "db/debug1"
             #:exists 'truncate
             (lam (out)
               (for ([m (cdr result)])
                 ;; Display for human
                 (dm (detach m '[0]) out)
                 (newline out)))))]))))

(def (round2)
  (call-with-input-file "db/data1"
    (lam (in)
      (match (read in)
        [(cons mixed unmixed)
         (let ([result (main mixed unmixed)])
           (displayln (length mixed))
           (displayln (length unmixed))
           (displayln (length (cdr result)))
           (displayln (length (car result)))
           ;; Write the db/data
           (call-with-output-file "db/data2"
             #:exists 'truncate
             (lam (out)
               (write result out)))

           (call-with-output-file "db/debug2"
             #:exists 'truncate
             (lam (out)
               (for ([m (cdr result)])
                 ;; Display for human
                 (dm (detach m '[0]) out)
                 (newline out)))))]))))

(def (round3)
  (call-with-input-file "db/data2"
    (lam (in)
      (match (read in)
        [(cons mixed unmixed)
         (let ([result (main mixed unmixed)])
           (displayln (length mixed))
           (displayln (length unmixed))
           (displayln (length (cdr result)))
           (displayln (length (car result)))
           ;; Write the db/data
           (call-with-output-file "db/data3"
             #:exists 'truncate
             (lam (out)
               (write result out)))

           (call-with-output-file "db/debug3"
             #:exists 'truncate
             (lam (out)
               (for ([m (cdr result)])
                 ;; Display for human
                 (dm (detach m '[0]) out)
                 (newline out)))))]))))

(round3)
