(import (chezscheme)
        (kara-lang main)
        (mol))
(load "types.ss")
(load "enum.ss")
(load "stream.ss")

;;; State variables


;;; Parameters (files are preferably strings)
(define db
  (map (lambda (x)  `(=> (f) (f) ,x))
       (append equality category)))

(define max-size 50)

(define banned-data
  #|data not allowed in the hypotheses|#
  '(= ->))

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

(define proof-steps
  (lambda (proof)
    (mol-< (ref proof '[0])
           (lambda (_) 0)
           (lambda (data _)
             (if (eq? data 'f)  0
                 (+ 1
                    (proof-steps (ref proof '[0]))
                    (proof-steps (ref proof '[1]))))))))

(define get-ungrounded
  (lambda (proof)
    (mol-< (ref proof '[0])
           (lambda (_)  (list (conclusion proof)))
           (lambda (data _)
             (if (eq? data 'f)  '()
                 (append (get-ungrounded (ref proof '[0]))
                         (get-ungrounded (ref proof '[1]))))))))

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
      (s-append (#|Assume conclusion|#
                 cond [(and (not (equal? (conclusion root)
                                     (conclusion (ref root path))))
                          (mol-< (ref root `[,@path 2])
                                 (lambda (_)       #f)
                                 (lambda (data _)  (not (memq data banned-data)))))
                       (s-cons root '())]
                      [else                '()])
                (#|Substitute axiom|#
                 >> (s-map (l> up root path)
                           (apply stream db))
                    (l> s-filter
                        (f>> (negate cycle?))))
                (#|Grinding with modus ponens|#
                 let ([root  (up root `[,@path] mp)])
                  (if (cycle? root)  '()
                      (delay  #|Crucial delay to avoid infinite loop|#
                        (begin (pydisplay "Hello")
                               (pydisplay (s-flatmap (lambda (x) (s-cons x '()))
                                                     (loop root `[,@path 0])))
                               (pydisplay "Mark 1")
                               (let ([res  (s-flatmap (f> loop `[,@path 1])
                                                      (loop root `[,@path 0]))])
                                 (pydisplay "Mark 2")
                                 (if (null? res)  '()
                                     (force res)))))))))))

;;; Jobs
(define b (bleed))

;; (do ([i 1 (+ i 1)])
;;     [(or (null? b)
;;         (> i 100))]
;;   (>> (let ([res  (s-car b)])
;;         (set! b (s-cdr b))
;;         res)
;;       (lambda (x)
;;         (pydisplay (>> x shorten clean))
;;         (pydisplay (proof-steps x)))))
