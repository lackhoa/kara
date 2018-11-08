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

(define db-no-sym
  (filter (negate (f> member '((=> (f) (f) (-> (= 1 0) (= 0 1))))))
          db))

(define max-steps 10)
(define trim?     #f)

(define banned-data
  #|data not allowed in the hypotheses|#
  '(= ->))

;;; Main Routines
(define cycle?
  #|Test whether or not we're encountering goal duplication|#
  (lambda (root)
    (let cloop ([mol   root]
                [seen  (list)])
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
  #|Counts how many => signs there are|#
  (lambda (proof)
    (mol-< proof
           (lambda (_) 0)
           (lambda (data _)
             (case data
               [f  0]
               [=>  (+ 1
                      (proof-steps (ref proof '[0]))
                      (proof-steps (ref proof '[1])))])))))

(define get-ungrounded
  (lambda (proof)
    (mol-< (ref proof '[0])
           (lambda (_)  (list (conclusion proof)))
           (lambda (data _)
             (case data
               [f  (list)]
               [=>  (append (get-ungrounded (ref proof '[1]))
                           (get-ungrounded (ref proof '[0])))])))))

(define get-antes
  (lambda (formula)
    (let loop ([formulae formulae]
               [res      (list)])
      (mol-< formulae
             (lambda (_)  res)
             (lambda (data _)
               (case data
                 [->     (loop (ref formulae '[1])
                              (cons (ref formulae '[0])
                                    res))]
                 [else  res]))))))

(define trim
  (lambda (proof)
    (let ([assumptions  (>> (get-ungrounded proof)
                            strip-duplicates)])
      (let builder ([asmps  assumptions])
        (if (null? asmps)  (conclusion proof)
            `(-> ,(car asmps)
                ,(builder (cdr asmps))))))))

(define b
  ;; The main stream
  (let loop ([proof  '(=> 0 1 2)]
             [path  '[]])
    (s-append
     (delay
       (#|Just assume (base case #1)|#
        cond [(mol-< (conclusion (ref proof path))
                     (lambda (_)       #f  #|Don't assume random propositions|#)
                     (lambda (data _)  (not (memq data banned-data)
                                     #|Don't assume dumb things|#)))
              (cons proof s-null)]
             [else                 (list)]))

     (#|Try an axiom (base case #2)|#
      delay
       (let ([candidates  (case path
                            [([])   (list)]
                            [([0])  db-no-sym]
                            [else   db])])
         (force (>> (s-map (l> up proof path)
                           (apply stream candidates))
                    (l> s-filter
                        (f>> (negate cycle?)))))))

     (delay
       #|Grinding with modus ponens (recursive case)|#
       (#|Must limit size for complete search|#
        if (>= (proof-steps proof) max-steps)  (list)
           (let ([proof  (up proof  `[,@path]  mp)])
             (if (cycle? proof)  (list)
                 (force (s-flatmap (f> loop   `[,@path 1])
                                   (loop proof `[,@path 0]))))))))))


;;; Tracing Business


;;; Jobs
(do ([i 1 (+ i 1)])
    [(or (s-null? b)
        (> i 100))]
  (>> (let ([res  (s-car b)])
        (set! b (s-cdr b))
        res)
      (lambda (x)
        (pydisplay (>> x (if trim? trim identity) clean))
        (pydisplay (proof-steps x) "steps"))))
