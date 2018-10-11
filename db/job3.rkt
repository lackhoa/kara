#lang racket
(require "../lang/kara.rkt"
         "../mol.rkt"
         "../enum.rkt"
         "../types.rkt"
         "../main.rkt")

;;; Redistributing theorems between two databases

(def db1 (file->list "data1.rkt"))
(def db2 (file->list "data2.rkt"))

(let* ([merge      (append db1 db2)]
       [avg-len    (round (/ (length merge) 2))]
       [new-merge  (shuffle merge)])
  (let-values ([(new-db1 new-db2)
                (split-at merge avg-len)])
    (call-with-output-file "data1.rkt"
      #:exists 'truncate
      (lam (out)
        (for ([m  new-db1])
          (wm m out))))

    (call-with-output-file "data2.rkt"
      #:exists 'truncate
      (lam (out)
        (for ([m  new-db2])
          (wm m out))))))
