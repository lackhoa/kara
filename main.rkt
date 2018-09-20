#! /usr/bin/racket
#lang racket

(require "lang/kara.rkt"
         "mole.rkt"
         "enum.rkt"
         "types.rkt")

(def (seed file-name)
  (call-with-output-file file-name
    #:exists 'truncate
    (lam (out) (write axioms out))))

(def (mix&react times)
  (let ([db  (call-with-input-file "db/data"
               (lam (in) (read in)))])
    (repeat times
            (thunk (set! db (main db))))
    (call-with-output-file "db/data"
      #:exists 'truncate
      (lam (out)
        (write db out)))))

(def (view-data)
  (call-with-input-file "db/data"
    (lam (in)
      (call-with-output-file "db/view"
        #:exists 'truncate
        (lam (out)
          (for ([m  (read in)])
            (dm (detach m '[0]) out)
            (newline out)))))))

(mix&react 1)
(view-data)
