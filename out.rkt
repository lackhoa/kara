#! /usr/bin/racket
#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "enum.rkt"
         "types.rkt")

(def (view)
  (call-with-input-file "db/data.rkt"
    (lam (in)
      (call-with-output-file "db/view.rkt"
        #:exists 'truncate
        (lam (out)
          (for ([m  (read in)])
            (dm (copy m '[0])
                out)
            (newline out)))))))

(view)
