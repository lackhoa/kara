(import (chezscheme)
        (kara-lang main)
        (mol))
(load "types.ss")
(load "enum.ss")
(load "stream.ss")

;;; State
(define db
  (map (lambda (x)  `(=> (f) (f) ,x))
       (append equality category)))

(define db-len
  (length db))

;;; Parameters (files are preferably strings)
(define max-size (make-parameter 130))


;;; Main Routines
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
                        (or (loop (car kids)  new-seen  #|First premise|#)
                           (loop (cadr kids) new-seen  #|Second premise|#)))]))))))

(define get-ungrounded
  (lambda (proof)
    (mol-< (ref proof '[0])
           (lambda (_)  (list (conclusion proof)))
           (lambda (data _)
             (if (eq? data 'f)  '()
                 (append (get-ungrounded (ref proof '[0]))
                         (get-ungrounded (ref proof '[1]))))))))

(define assume-much?
  (lambda (thm)
    (mol-< thm
           (lambda (_) #f)
           (lambda (data kids)
             (and (eq? data '->)
                (or (mol-< (list-ref kids 0)
                          (lambda (_) #t)
                          (lambda (data _) (eq? data '->)))
                   (assume-much? (list-ref kids 1))))))))

(define shorten
  (lambda (proof)
    (let ([assumptions  (>> (get-ungrounded proof)
                            strip-duplicates)])
      (let builder ([asmps  assumptions])
        (if (null? asmps)  (conclusion proof)
            `(-> ,(car asmps)
                ,(builder (cdr asmps))))))))

(define bleed
  ;; Returns: a stream
  (lambda ()
    (let loop ([root  '(=> 0 1 2)]
               [path  '[]])
      (s-append (cond [(and (not (equal? (conclusion root)
                                     (conclusion (ref root path))))
                          (mol-< (ref root `[,@path 2])
                                 (lambda (_)       #f)
                                 (lambda (data _)  (and (not (eq? data '->))
                                                 (not (eq? data '=))))))
                       (s-cons root '())  #|Assume conclusion|#]
                      [else  '()])
                (>> (map (l> up root path)
                         db)
                    (l> filter (lambda (x)
                                 (and x (not (cycle? x)))))
                    list->stream  #|Substitute axiom|#)
                (if (> (size root) (max-size))  '()
                    (>> (up root `[,@path] mp)
                        (pass (negate cycle?))
                        (f> loop `[,@path 0])
                        (l> s-flatmap
                            (f> loop `[,@path 1]))))))))

;;; Jobs
(define b (bleed))
(do ([i 1 (+ i 1)])
    [(or (null? b)
        (> i 1000))]
  (>> (let ([res  (s-car b)])
        (set! b (s-cdr b))
        res)
      shorten
      clean
      pydisplay))
