#! /usr/bin/racket
#lang racket

(require "lang/kara.rkt"
         "mole.rkt"
         "enum.rkt"
         "types.rkt")

(def (seed file-name)
  (call-with-output-file file-name
    #:exists 'truncate
    (lam (out) (wm axioms out))))

(def (mix&react times)
  (let ([db  (call-with-input-file "db/data.rkt"
               (lam (in) (read in)))])
    (repeat times
            (thunk (set! db (main db))))
    (call-with-output-file "db/data.rkt"
      #:exists 'truncate
      (lam (out)
        (wm db out)))))

(def (view)
  (call-with-input-file "db/data.rkt"
    (lam (in)
      (call-with-output-file "db/view.rkt"
        #:exists 'truncate
        (lam (out)
          (for ([m  (read in)])
            (dm (copy m '[0]) out)
            (newline out)))))))

;; (seed "db/data.rkt")
(mix&react 1)
(view)
