(import (kara-lang main))

(define ev
  (lambda (e)
    (cond [(and (eq? (car e) '::)
              (eq? (cadr e) 'quote))
           (cadr e)]
          [(and (eq? (car e) '::)
              (eq? (cadr e) 'call))
           (ap (ev (caddr e))
               (ev (cadddr e)))]
          [else
           (error "Eval" "Invalid expression" e)])))

(define ap
  (lambda (proc arg)
    (define loop
      (trace-lambda loop (x)
        (cond [(eq? (car x)
                    'var)    (if (zero? (cadr x))  arg
                                 `(var ,(1- (cadr x))))]
              [(eq? (car x)
                    'atom)   x]
              [(eq? (car x)
                    '::)     `(:: ,(loop (cadr x))
                                  ,(loop (caddr x)))]
              [else
               (error "Apply" "OMG what is this?" proc)])))

    (ev (loop (cadr proc  #|function body|#)))))

(trace ev)
(trace ap)

(define p1
  '(:: call
       (:: '(lambda (:: (atom quote) (var 0)))
           '(:: (atom c) (atom ())))))
(ev p1)
