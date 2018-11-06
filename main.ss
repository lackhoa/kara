(import (chezscheme)
        (kara-lang main))
(load "types.ss")
(load "enum.ss")
(load "stream.ss")
(load "mol.ss")

;;; State variables


;;; Parameters (files are preferably strings)
(define db
  (map (lambda (x)  `(=> (f) (f) ,x))
       (append equality category)))

(define max-steps 19  #|19 is the magic number|#)

(define banned-data
  #|data not allowed in the hypotheses|#
  '(= ->))

;;; Main Routines
(define cycle?
  #|Test whether or not we're encountering goal duplication|#
  (lambda (root)
    (let cloop ([mol   root]
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
                        (or (cloop (car kids)  new-seen  #|First premise|#)
                           (cloop (cadr kids) new-seen  #|Second premise|#)))]))))))

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

(define b
  ;; The main stream
  (let loop ([root  '(=> 0 1 2)]
             [path  '[]])
    (s-append
     (#|Just assume (base case #1)|#
      cond [(mol-< (conclusion (ref root path))
                   (lambda (_)       #f  #|Don't assume random propositions|#)
                   (lambda (data _)  (not (memq data banned-data))))
            (stream root)]
           [else            s-null])

     (#|Try an axiom (base case #2)|#
      >> (s-map (l> up root path)
                (apply stream db))
         (l> s-filter
             (f>> (negate cycle?))))

     (delay
       #|Grinding with modus ponens (recursive case)|#
       (#|Must limit size for complete search|#
        if (>= (proof-steps root) max-steps)  '()
           (let ([root  (up root  `[,@path]  mp)])
             (if (cycle? root)  '()
                 (force (s-flatmap (f> loop   `[,@path 1])
                                   (loop root `[,@path 0]))))))))))

;;; Tracing Business


;;; Jobs
(do ([i 1 (+ i 1)])
    [(or (null? b)
        (> i 100))]
  (>> (let ([res  (s-car b)])
        (set! b (s-cdr b))
        res)
      (lambda (x)
        (pydisplay (>> x shorten clean))
        (pydisplay (proof-steps x)))))
