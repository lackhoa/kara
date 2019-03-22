(define make-s (lambda (u v) `(,u ,v)))
(define lhs car)
(define rhs cadr)
(define extend (lambda (env l r) `(,(make-s l r) . ,env)))
