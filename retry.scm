(define retry #f)

(define (fact n)
  (if (= n 0)
      (call/cc (lambda (k) (set! retry k) 1))
      (* n (fact (- n 1)))))

(define x
  (call/cc (lambda (k) k)))
(x (lambda (ignore) "hi"))

(define lwp-list '())
(define (lwp thunk)
  (set! lwp-list (append lwp-list (list thunk))))

(define (start)
  (define p (car lwp-list))
  (set! lwp-list (cdr lwp-list))
  (p))

(define (pause)
  (call/cc
   (lambda (k)
     (lwp (lambda () (k #f)))
     (start))))

(lwp (lambda () (let )))
