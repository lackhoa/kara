(import (chezscheme)
        (kara-lang main)
        (mol))
(load "types.ss")
(load "enum.ss")

;;; State
(define db (append equality category))
(define current #f)
(define blacklist '())

(define load!
  (lambda ()
    (set! db
      (read (open-input-file (db-file))))

    (set! blacklist
      (read (open-input-file (bl-file))))))

(define save
  (lambda ()
    (when (file-exists? (db-file))
      (delete-file (db-file)))
    (with-output-to-file (db-file)
      (lambda () (write db)))
    (when (file-exists? (bl-file))
      (delete-file (bl-file)))
    (with-output-to-file (bl-file)
      (lambda () (write blacklist)))))

(define bl
  (lambda ()
    (set! blacklist (cons current blacklist))))

;;; Parameters (files are preferably strings)
(define db-file    (make-parameter "db/data.db"))
(define bl-file    (make-parameter "db/bl.db"))
(define view-file  (make-parameter "log/view.ss"))
(define size-limit (make-parameter 1000))
(define dice       (make-parameter (cons #f (make-list 10 #t))))

(define store!
  (lambda () (set! db (cons current db))))

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
                        (or (loop (car kids) new-seen  #|First premise|#)
                           (loop (cadr kids) new-seen  #|Second premise|#)))]))))))

(define shorten
  (lambda (proof)
    (let builder
        ([assumptions
          (let gather-ungrounded ([pr proof])
            (mol-< (ref pr '[0])
                   (lambda (_)  (list (conclusion pr)))
                   (lambda (data _)
                     (if (eq? data 'f)  '()
                         (append (gather-ungrounded (ref pr '[0]))
                                 (gather-ungrounded (ref pr '[1])))))))])
      (if (null? assumptions)  (conclusion proof)
          `(-> ,(car assumptions)
              ,(builder (cdr assumptions)))))))

(define ma
  (lambda ()
    (>> (let loop ([root  mp]
                   [path  '[]])
          (let ([candidates
                 (#|Check if there is an axiom waiting|#
                  >> (map (l> up root `[,@path 2])
                          db)
                     (l> filter (f>> (negate cycle?))))])
            (cond [(and (null? candidates)
                      (not (eq? (>> (ref root path)
                                  conclusion
                                  car)
                              '->)))
                   root]
                  [(and (not (null? candidates))
                      (not (equal? path '[]))
                      (not (ran-elem (dice))))
                   (>> (ran-elem candidates)
                       (f> up `[,@path 0] '(f))
                       (f> up `[,@path 1] '(f))
                       #|Knock off the hypotheses|#)]
                  [(< (size root) (size-limit))
                   (#|Keep grinding with modus ponens|#
                    >> (up root path mp)
                       (f> loop `[,@path 0])
                       (f> loop `[,@path 1]))]
                  [else  #f])))
        shorten
        clean)))

(define go
  (lambda ()
    (do ([res #f (ma)])
        [(>> res
             (lambda (x)
               (andmap (lambda (thm)
                         (not (instance? res thm)))
                       (append db blacklist))))
         (set! current res)
         res])))

;;; Jobs
;; (load!)
