(import (chezscheme)
        (kara-lang main)
        (mol))
(load "types.ss")

;;; State
(define db (append equality category))
(define current #f)

(define load!
  (lambda () (read (open-input-file (db-file)))))

(define save
  (lambda () (with-output-to-file (db-file)
          (lambda () (write db)))))

;;; Parameters (preferably strings)
(define db-file    (make-parameter "db/data.db"))
(define view-file  (make-parameter "log/view.ss"))

(define store!
  (lambda () (set! db `(,current ,@db))))

;;; Functions
(define ran-elem
  (lambda (ls)
    (list-ref ls  (random (length ls)))))

(define ma
  (lambda ()
    ;; Choose one of the options: a. modus ponens, b. apply an axiom form
    (clean
     (let loop ([root  mp]
                [path  '[]])
       (cond [(and (not (equal? path '[]))
                 (ran-elem '(#t #f)))
              (>> (map (l> up root `[,@path 2])
                       db)
                  filter-false
                  (lambda (ls) (if (null? ls) root
                              (ran-elem ls))))]

             [else  (>> (up root path mp)
                        (f> loop `[,@path 0])
                        (f> loop `[,@path 1]))])))))
