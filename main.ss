(library (main-procs)
  (export main load! save store!)
  (import (chezscheme) (kara-lang main)
          (mol) (enum) (types))

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

  (define shorten
    (lambda (ent)
      (define gather-nongrounded
        (lambda (ent)
          (if (not (ent-prem ent))  (list ent)
              (flatmap gather-nongrounded
                       (ent-prem ent)))))

      (ent% (gather-nongrounded ent)
            (ent-ccs ent))))

  (define rmain
    (lambda (root)
      (define cycle?
        (lambda (root path)
          (do ([p1    '[]   (append p1 (list-head p2 2))]
               [p2    path  (list-tail p2 2  #|2 is to skip to the premise|#)]
               [seen  '()   (cons (ent-ccs (ref root p1))
                                  seen)])
              ((null? p2)
               (ormap (f>> ent-ccs (f> member seen) bool)
                      (ent-prem (ref root path)))))))

      (define extend
        ;; fill in an open entailment, #f if no method is available
        (lambda (root path)
          (let ([candidates
                 (filter (f>> (negate (f> cycle? path)))
                         (map (l> up root path) db))])
            (if (null? candidates) #f
                (ran-elem candidates)))))

      (let loop ([root  root]
                 [path  '[]])
        (let ([pnum
               (length (ent-prem (ref root path)))])
          (do ([i   0
                    (+ i 1)]
               [res root
                    (let ([p  `(,@path 0 ,i)  #|To premise i|#])
                      (cond [(ent-prem (ref res p))
                             (loop res p)  #|Already filled, go down|#]

                            [(ran-elem '(#f #t #t))
                             (let ([ext  (extend res p)])
                               (if ext (loop ext p)
                                   res))  #|Fill in a method|#]

                            [else  res  #|Bad roll|#]))])

              ((>= i pnum)  res))))))

  (define main
    (lambda ()
      (let* ([chosen  (ran-elem
                       (filter (lambda (ent)
                                 (not (null? (ent-prem ent))))
                               db))]
             [res     (>> chosen rmain shorten clean)])
        (set! current res)
        res))))

(import (chezscheme) (kara-lang main)
        (enum) (main-procs))

(debug? #f)
