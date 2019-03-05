(define make-obj (lambda (type val) `(type val)))
(define obj->type car)
(define obj->val cadr)
