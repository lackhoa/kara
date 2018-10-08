#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "enum.rkt"
         "types.rkt")
(provide (all-defined-out))

(print-graph #t)

(def db
  (match (file-exists? "db/data.rkt")
    [#t  (file->list "db/data.rkt")]
    [#f  (map compress axioms)]))

(def (seed)
  (set! db (map compress axioms)))

(def (num)
  (length db))

(def (com [times 100])
  (repeat times
          (thunk (set! db (combine db)))))

(def (col [times 10])
  (repeat times
          (thunk (set! db (collide db)))))

(def (save [filename "db/data.rkt"])
  (call-with-output-file filename
    #:exists 'truncate
    (lam (out)
      (for ([m  db])
        (wm m out)))))

(def (view)
  (call-with-output-file "db/view.rkt"
    #:exists 'truncate
    (lam (out)
      (for ([m  db])
        (dm m out)
        (newline out)))))

(def (query thm)
  (for/or ([m  (map decompress db)])
    (match (instance? thm (conclusion m))
      [#t  (dm m)]
      [#f  #f])))
