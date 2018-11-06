(import (kara-lang main))

(define s-null
  ;; The problem of testing nullness belongs to the investigator, not the creator
  (delay '()))

(define s-null?
  (lambda (s)
    (null? (force s))))

(define s-append
  (lambda ss
    (if (null? ss)  s-null
        (let ([s1   (car ss)]
              [s23  (cdr ss)])
          (if (s-null? s1)  (apply s-append s23)
              (delay (cons (s-car s1)
                           (apply s-append
                             (s-cdr s1) s23))))))))

(define stream
  (lambda ls
    (if (null? ls)  s-null
        (delay (cons (car ls)
                     (apply stream (cdr ls)))))))

(define s-car
  (lambda (s)
    (car (force s))))

(define s-cdr
  (lambda (s)
    (cdr (force s))))

(define s-ref
  (lambda (s n)
    (if (eq? n 0)  (s-car s)
        (s-ref (s-cdr s) (- n 1)))))

(define s-map
  (lambda (proc s)
    (if (s-null? s)  s-null
        (delay (cons (proc (s-car s))
                     (s-map proc (s-cdr s)))))))

(define s-flatmap
  (lambda (proc s)
    (if (s-null? s)  s-null
        (delay (force (s-append (proc (s-car s))
                                (s-flatmap proc (s-cdr s))))))))

(define stream->list
  (lambda (s)
    (let loop ([s      s]
               [accum  '()])
      (if (s-null? s)  (reverse accum)
          (loop (s-cdr s)
                (cons (s-car s) accum))))))

(define s-filter
  (lambda (pred s)
    (delay
      (cond [(s-null? s)       '()]
            [(pred (s-car s))  (cons (s-car s)
                                     (s-filter pred (s-cdr s)))]
            [else              (force
                                (s-filter pred (s-cdr s)))]))))

;;; Testing
(define counters
  (let next ([n 0])
    (if (> n 10)  s-null
        (delay (cons n (next (+ n 1)))))))

(define nat
  (let next ([n 0])
    (delay (cons n (next (+ n 1))))))

(define even-counters
  (s-filter even? (s-append counters counters)))

(define even-nat
  (s-filter even? (s-append counters nat)))
