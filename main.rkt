#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "enum.rkt"
         "types.rkt")
(provide (all-defined-out))

(print-graph #t)

(def db (map compress axioms))

(def (load filename)
  (set! db (file->list filename)))

(def (num)
  (length db))

(def (com [times 100])
  (repeat times
          (thunk (set! db (combine db)))))

(def (col [times 10])
  (repeat times
          (thunk (set! db (collide db)))))

(def (save filename)
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
        (dm (first (cmol%-kids m)) out)
        (newline out)))))

(def (query thm)
  (for/or ([m  (map decompress db)])
    (match (instance? thm (conclusion m))
      [#t  (dm m)]
      [#f  #f])))
