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

;;; Batch jobs
(let ([flag  (command-line #:args ([flag "0"]) flag)])
  (case flag
    [("0")      (void)]

    [("1" "2")  (let ([FILE-NAME  (match flag
                                    ["1"  "db/data1"]
                                    ["2"  "db/data2"])])
                  (load FILE-NAME)
                  (for ([i  (in-naturals)])
                    (pydisplay "Cycle number:" i)
                    (time (com 10))
                    (time (col 1))
                    (displayln (num))
                    (save FILE-NAME)))]

    [("3")      (let* ([db1        (file->list "db/data1")]
                       [db2        (file->list "db/data2")]
                       [merge      (append db1 db2)]
                       [avg-len    (round (/ (length merge) 2))]
                       [new-merge  (shuffle merge)])
                  (let-values ([(new-db1 new-db2)
                                (split-at new-merge avg-len)])
                    (call-with-output-file "db/data1"
                      #:exists 'truncate
                      (lam (out)
                        (for ([m  new-db1])
                          (wm m out))))

                    (call-with-output-file "db/data2"
                      #:exists 'truncate
                      (lam (out)
                        (for ([m  new-db2])
                          (wm m out))))))]))
