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

;;; Main Routines
(define ran-elem
  (lambda (ls)
    (list-ref ls  (random (length ls)))))

(define cycle?
  #|Test whether or not we're encountering goal duplication|#
  (lambda (root)
    (let loop ([mol   root]
               [seen  '()])
      (mol-< mol
             (lambda (_)  #f)
             (lambda (data kids)
               (cond [(eq? data 'f)             #f]
                     [(member (conclusion mol)
                              seen)             #t]
                     [else
                      (let ([new-seen  (cons (conclusion mol)
                                             seen)])
                        (or (loop (car kids) new-seen)
                           (loop (cadr kids) new-seen)))]))))))

(define ma
  (lambda ()
    ;; Choose one of the options: a. modus ponens, b. apply an axiom form
    (clean
     (let loop ([root  mp]
                [path  '[]])
       (cond [(and (not (equal? path '[]))
                 (ran-elem '(#t #f)))
              (#|Try a random axiom and be done with it|#
               >> (map (l> up root `[,@path 2])
                       db)
                  (l> filter (f>> (negate cycle?)))
                  (lambda (ls)
                    (if (null? ls)  root  #|Leave the hypotheses open|#
                        (>> (ran-elem ls)
                            (f> up `[,@path 0] '(f))
                            (f> up `[,@path 1] '(f))
                            #|Knock off the hypotheses|#))))]

             [else  (#|Keep grinding with modus ponens|#
                     >> (up root path mp)
                        (f> loop `[,@path 0])
                        (f> loop `[,@path 1]))])))))

(define m (lambda () (ma) 0))
