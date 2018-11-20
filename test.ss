(import (kara-lang main))

(define ap
  (lambda (exp arg)
    (define ap-core
      (lambda (exp)
        (case (car exp)
          [bind   `(bind ,(ap-core (cadr exp)))]
          [*      (if (not (equal? exp '(*)))  (cdr exp)
                      `(const ,arg  #|note the const|#))]
          [const  exp]
          [::     `(:: ,(ap-core (cadr exp))
                       ,(ap-core (caddr exp)))]
          [else   (error "ap" "Invalid expression" exp)])))

    (ap-core (cadr exp  #|Predicate body|#))))

(define decode
  (lambda (exp)
    (let loop ([exp  exp])
      (case (car exp)
        [const   (cadr exp)]
        [::      (cons (loop (cadr exp))
                       (loop (caddr exp)))]
        [bind    (error "print" "Let's leave this to Kara")]))))

(define e
  '(bind (bind (*))))
(define e2
  '(bind (bind (:: (* *) (*)))))
(define e3
  '(bind (bind (bind (:: (:: (const K) (* * *))
                         (:: (* * *) (:: (* *) (:: (*) (const ())))))))))

(pydisplay (decode (ap (ap e 'x) 'y)))
(pydisplay (decode (ap (ap e2 'x) 'y)))
(pydisplay (decode (ap (ap (ap e3 'x) 'y) 'z)))
