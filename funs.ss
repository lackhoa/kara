(define pp pretty-print)
(define ppl (lambda (ls) (for-each pp ls)))

(define repeat-func
  (lambda (i f)
    (unless (= i 0) (f) (repeat-func (- i 1) f))))
(define-syntax repeat
  (syntax-rules ()
    [(_ i e)
     (repeat-func i (lambda () e))]))

(define-syntax reflect
  ;; Goal for debugging
  (syntax-rules ()
    [(_ msg x)
     (project (x) (begin (pp msg) (pp x) succeed))]))

(define remove-duplicates
  (lambda (ls)
    (cond
     [(null? ls) '()]
     [(let ([a (car ls)]
            [d (cdr ls)])
        (cond
         [(memq a d) (remove-duplicates d)]
         [else `(,a . ,(remove-duplicates d))]))])))
