(import (chezscheme)
        (kara-lang main)
        (mol))
(load "types.ss")

;;; State
(define db (append equality))
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

(define cycle?
  #|Test whether or not the conclusion at path is already seen|#
  (lambda (root path)
    (do ([m     root   (ref m `[,(car p)])]
         [p     path   (cdr p)]
         [seen  '()    (cons (conclusion m)
                             seen)])
        [(null? p)
         (>> (member (conclusion m)
                     seen)
             bool)])))

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
                  (l> filter
                      (lambda (x)  (and x (not (cycle? x path)))))
                  (lambda (ls) (if (null? ls) root  #|Leave the hypotheses open|#
                              (>> (ran-elem ls)
                                  (f> up `[,@path 0] '(f))
                                  (f> up `[,@path 1] '(f))
                                  #|Knock off the hypotheses|#))))]

             [else  (>> (up root path mp)
                        (f> loop `[,@path 0])
                        (f> loop `[,@path 1]))])))))
