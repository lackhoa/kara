(import (chezscheme)
        (kara-lang main))
(load "types.ss")
(load "enum.ss")
(load "stream.ss")
(load "mol.ss")

;;; State variables


;;; Parameters (files are preferably strings)
(define db
  (append evaluation))

(define boring
  (list (mk-proof '()
                  '(= 0 0))
        (mk-proof '((= 1 0))
                  '(= 0 1))
        (mk-proof '((and 0 1))
                  '0)
        (mk-proof '((and 0 1))
                  '1)))

(define max-steps 6)
(define trim?     #t)
(define show-num  100)

(define banned-data
  #|data not allowed in the hypotheses|#
  '(=))

;;; Main Routines
(define cycle?
  #|Test whether or not we're encountering goal duplication|#
  (lambda (proof)
    (let loop ([mol   proof]
               [seen  (list)])
      (mol-< mol
             (lambda _  #f)
             (lambda _  (or (bool (member (get-ccs mol)
                                    seen))
                      (ormap (f> loop (cons (get-ccs mol)
                                            seen))
                             (get-prem mol))))))))

(define proof-steps
  #|Counts how many => signs there are|#
  (lambda (proof)
    (mol-< proof
           (lambda _ 0)
           (lambda _
             (fold-left + 1 (map proof-steps (get-prem proof)))))))

(define get-ungrounded
  (lambda (proof)
    (mol-< (ref proof '[0])
           (lambda (_)  (list (get-ccs proof)))
           (lambda (data _)
             (flatmap get-ungrounded
                      (get-prem proof))))))

(define trim
  (lambda (proof)
    (let ([assumptions  (>> (get-ungrounded proof)
                            strip-duplicates)])
      `(:- (list ,@assumptions) ,(get-ccs proof)))))

(define main
  (lambda (proof rel index)
    #|`rel` points to the focused premise list, `index` is that list's index|#
    (let ([#|points to the first premise|#
           path  `[,@rel ,index]])
      (#|If path is invalid then the list is empty -> nothing to do|#
       if (not (ref proof path))  (stream proof)
          (s-append
           (delay
             (#|Just assume it (base case)|#
              cond [(mol-< (get-ccs (ref proof path))
                           (lambda (_)       #f  #|Don't assume random propositions|#)
                           (lambda (data _)  (not (memq data banned-data)
                                           #|Don't assume dumb things|#)))
                    (cons proof s-null  #|careful with this!|#)]
                   [else                 (list)]))

           (#|Substitute an axiom (recursive case)|#
            delay
             (force (#|limit size for complete search|#
                     cond [(>= (proof-steps proof)
                              max-steps)  s-null]
                          [else
                           (s-flatmap  (#|continue on to the other premise ...|#
                                        f> main rel (+ index 1))
                                       (#|... after enumerating down|#
                                        >> (s-map (l> up proof path)
                                                  (apply stream db))
                                           (l> s-filter
                                               (f>> (negate cycle?)))
                                           (l> s-flatmap
                                               (f> main `[,@path 0] 0))))]))))))))

(define b
  ;; The main stream
  (s-flatmap (f> main '[0] 0)
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
