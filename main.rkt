#! /usr/racket/bin/racket
#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "enum.rkt"
         "types.rkt")

(def db
  (call-with-input-file "db/data.rkt"
    (lam (in) (read in))))

(def (total)
  (length db))

(def (seed [file-name "db/data.rkt"])
  (call-with-output-file file-name
    #:exists 'truncate
    (lam (out) (wm axioms out))))

(def (combine-n [times 100])
  (repeat times
          (thunk (set! db (combine db)))))

(def (collide-n [times 5])
  (repeat times
          (thunk (set! db (collide db)))))

(def (save [filename "db/data.rkt"])
  (call-with-output-file filename
    #:exists 'truncate
    (lam (out)
      (wm db out))))

(def (view)
  (call-with-output-file "db/view.rkt"
    #:exists 'truncate
    (lam (out)
      (for ([m  db])
        (dm (copy m '[0])
            out)
        (newline out)))))

(def (query thm)
  (for/or ([m  db])
    (match (instance thm (conclusion m))
      [#t  (dm m)]
      [#f  #f])))
