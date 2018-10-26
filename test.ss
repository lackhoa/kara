(import (kara-lang main))

(define N
  (lambda (b d)
    (nat-< d b
           (lambda (d-1) (+ b (expt (N b d-1) 2))))))

(define body
  (lambda (b d counter)
    (if (< counter b) counter  #|atomic phase|#
        (nat-< d  #f
               (lambda (d-1)
                 (let-values ([(i1 i2) (div-and-mod (- counter b  #|application phase|#)
                                                    (N b d-1))])
                   `(,(body b d-1 i1) ,(body b d-1 i2))))))))

(define gen
  (lambda (b d)
    (let ([limit (N b d)])
      (let loop ([i    0]
                 [res  '()])
        (if (>= i limit) res
            (loop (+ i 1)
                  `((lambda ,(iota b) ,(body b d i))
                    ,@res)))))))
