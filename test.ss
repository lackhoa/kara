(import (kara-lang main))

(define ap
  (lambda (exp arg)
    (define ap-core
      (lambda (exp)
        (case (car exp)
          [bind   `(bind ,(ap-core (cadr exp)))]
          [var    (if (zero? (cadr exp))  `(atom ,arg)
                      `(var ,(1- (cadr exp))))]
          [atom   exp]
          [::     `(:: ,(ap-core (cadr exp))
                       ,(ap-core (caddr exp)))]
          [else   (error "ap" "Invalid expression" exp)])))

    (ap-core (cadr exp  #|Predicate body|#))))

(define print
  (lambda (exp)
    (let loop ([exp  exp])
      (case (car exp)
        [atom  (cadr exp)]
        [::    (cons (loop (cadr exp))
                     (loop (caddr exp)))]
        [else  (error "print" "Can't print") exp]))))

(define e
  '(bind (bind (var 0))))
(define e2
  '(bind (bind (:: (var 1) (var 0)))))
(define e3
  '(bind (bind (bind (:: (:: (atom K) (var 2))
                         (:: (var 2) (:: (var 1) (:: (var 0) (atom ())))))))))

(pydisplay (print (ap (ap e 'x) 'y)))
(pydisplay (print (ap (ap e2 'x) 'y)))
(pydisplay (print (ap (ap (ap e3 'x) 'y) 'z)))
