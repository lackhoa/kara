(define-record-type clos (fields env var body))
(display (clos-env (make-clos 'env 'var 'body)))
