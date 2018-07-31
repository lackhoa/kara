#lang racket
(provide (all-defined-out))


; "lam" instead of "lambda"
(define-syntax-rule (lam whatever ...)
  (lambda whatever ...))

; "def" instead of "define"
(define-syntax-rule (def whatever ...)
  (define whatever ...))

; "def*" instead of "define-values"
(define-syntax-rule (def* (id ids ...) expr)
  (match-define (list id ids ...) expr))

; Pattern matching
(define-syntax case
  (syntax-rules (else)
    ; Else 
    [(_ [else e1 e2 ...]) (begin e1 e2 ...)]
    ; No cases left
    [(_ val) (error "Pattern matching" "Pattern matching failed" val)]
    ; Some case left
    [(_ val [(pat-con var ...) e1 e2 ...] rest ...)
     ; I wonder how the quotation on `pat-con` works?
     (if (eq? (car val) 'pat-con)
         (apply (lambda (var ...) b1-first b1-rest ...) (cdr val))
       (case val rest ...))]))

; Constant functions
(define-syntax-rule (const b)
  (lambda () b))


