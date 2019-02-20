;;; Parameters
(define weights
  (vector 58400 30000 58400 50000))
(define values
  (vector (+ (* 0.45 75) (* 0.25 60) (* 0.3 60))
          (+ (* 0.45 0)  (* 0.25 30) (* 0.3 100))
          (+ (* 0.45 30) (* 0.25 80) (* 0.3 80))
          (+ (* 0.45 0)  (* 0.25 10) (* 0.3 100))))
(define w-lim
  141700)

;;; Algorithm
(define wi
  (lambda (i)
    (vector-ref weights i)))
(define vi
  (lambda (i)
    (vector-ref values i)))

(define m
  (lambda (i w-lim)
    (cond
     [(= i -1) (list 0 '())]
     [(> (wi i) w-lim)
      (m (- i 1) w-lim)]
     [else
      (let* ([drop (m (- i 1) w-lim)]
             [drop-val (car drop)]
             [drop-chosen (cadr drop)]
             [take (m (- i 1) (- w-lim (wi i)))]
             [take-val (+ (car take) (vi i))]
             [take-chosen (cons i (cadr take))])
        (cond
         [(< drop-val take-val) (list take-val take-chosen)]
         [else (list drop-val drop-chosen)]))])))

;;; Running the program
(display
 (let ([number-of-items
        (- (vector-length weights) 1)])
   (m number-of-items w-lim)))
