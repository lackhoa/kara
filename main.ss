(import (chezscheme)
        (kara-lang main))
(load "types.ss")
(load "enum.ss")
(load "stream.ss")
(load "mol.ss")

;;; State variables


;;; Parameters (files are preferably strings)
(define db
  (append category equality))

(define boring
  (append (list-head equality 2)))

(define max-steps  6)
(define trim?      #t)
(define show-num   100)

(define banned-ctor
  #|constructors not allowed in the hypotheses|#
  '(= and))

;;; Main Routines
(define cycle?
  #|Test whether or not we're encountering goal duplication|#
  (lambda (proof)
    (let loop ([mol   proof]
               [seen  (list)])
      (mol-< mol
             (lambda _  #f) (lambda _  #f)
             (lambda _  (or (bool (member (get-ccs mol)
                                    seen))
                      (ormap (f> loop (cons (get-ccs mol)
                                            seen))
                             (get-prem mol))))))))

(define proof-steps
  #|Counts how many => signs there are|#
  (lambda (proof)
    (mol-< proof
           (lambda _ 0)  (lambda _ #f)
           (lambda _
             (fold-left + 1 (map proof-steps (get-prem proof)))))))

(define get-ungrounded
  (lambda (proof)
    (mol-< (ref proof '[cdr cdr]  #|Get premises list|#)
           (lambda _  (list (get-ccs proof)))
           (lambda _  #f)
           (lambda _  (flatmap get-ungrounded
                          (get-prem proof))))))

(define trim
  (lambda (proof)
    (let ([assumptions  (>> (get-ungrounded proof)
                            strip-duplicates)])
      `(=> ,(get-ccs proof) ,@assumptions))))

(define main
  (lambda (proof lpath)
    #|`lpath points to the list of remaining premises`|#
    (let ([path  `[,@lpath car  #|points to the current premise|#]])
      (#|If path is invalid then the list is empty -> nothing to do|#
       if (not (ref proof path))  (stream proof)
          (s-append
           (delay
             (#|Just assume it (base case)|#
              cond [(mol-< (get-ccs (ref proof path))
                           (lambda _     #f  #|Don't assume random propositions|#)
                           (lambda _     #f  #|Don't assume constants|#)
                           (lambda (pr)  (not (memq (car pr) banned-ctor)
                                       #|Don't assume dumb things|#)))
                    (cons proof s-null  #|careful with this!|#)]
                   [else                 (list)]))

           (#|Substitute an axiom (recursive case)|#
            delay
             (force
              (#|limit size for complete search|#
               cond [(>= (proof-steps proof)
                        max-steps)  s-null]

                    [else  (s-flatmap
                            (#|continue on to the other premise ...|#
                             f> main `[,@lpath cdr])
                            (#|... after enumerating down|#
                             >> (s-map (l> up proof path)
                                       (apply stream db))
                                (l> s-filter
                                    (f>> (negate cycle?)))
                                (l> s-flatmap
                                    (f> main `[#|premise of this proof|#
                                               ,@path cdr cdr]))))]))))))))

(define b
  ;; The main stream
  (s-flatmap (f> main '[cdr cdr  #|Top-level premise list|#])
             (apply stream
               (filter (negate (f> member boring))
                       db))))

;;; Tracing Business


;;; Jobs
(do ([i 1 (+ i 1)])
    [(or (s-null? b)
        (> i show-num))]
  (>> (let ([res  (s-car b)])
        (set! b (s-cdr b))
        res)
      (lambda (x)
        (pydisplay (>> x (if trim? trim identity) clean))
        (pydisplay (proof-steps x) "steps"))))
