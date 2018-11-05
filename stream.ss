(import (kara-lang main))

(define-syntax s-cons
  (syntax-rules ()
    [(_ item s)
     (delay (cons item s))]))

(define s-car
  (lambda (s)
    (car (force s))))

(define s-cdr
  (lambda (s)
    (cdr (force s))))

(define s-append
  (lambda ss
    (if (null? ss)  '()
        (let ([s1   (car ss)]
              [s23  (cdr ss)])
          (if (null? s1)  (apply s-append s23)
              (s-cons (s-car s1)
                      (apply s-append
                        (s-cdr s1) s23)))))))

(define s-map
  (lambda (proc s)
    (if (null? s)  '()
        (s-cons (proc (s-car s))
                (s-map proc (s-cdr s))))))

(define s-flatmap
  (lambda (proc s)
    (if (null? s)  '()
        (s-append (proc (s-car s))
                  (s-flatmap proc (s-cdr s))))))

(define (stream->list s)
  (if (null? s)  '()
      (cons (s-car s)
            (stream->list (s-cdr s)))))

(define list->stream
  (lambda (ls)
    (list-< ls  '()
            (lambda (ls-car ls-cdr)
              (s-cons ls-car
                      (list->stream ls-cdr))))))

(define s-filter
  (lambda (pred s)
    (cond [(null? s)         '()]
          [(pred (s-car s))  (s-cons (s-car s)
                                     (s-filter pred (s-cdr s)))]
          [else              (s-filter pred (s-cdr s))])))

;;; Testing
(define counters
  (let next ([n 0])
    (if (> n 10)  '()
        (s-cons n (next (+ n 1))))))

(define s-add
  (lambda (s1 s2)
    (s-cons
     (+ (s-car s1) (s-car s2))
     (s-add (s-cdr s1) (s-cdr s2)))))

(define even-counters
  (s-add counters counters))

(s-car even-counters)
(s-car (s-cdr even-counters))
