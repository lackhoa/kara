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
    [(_ val [else e1 e2 ...]) (begin e1 e2 ...)]
    ; No cases left
    [(_ val) (error "Pattern matching" "No match found" val)]
    ; Some case left
    [(_ val [(pat-con var ...) e1 e2 ...] rest ...)
     ; I wonder how the quotation on `pat-con` works?
     (if (eq? (car val) 'pat-con)
         (apply (lambda (var ...) e1 e2 ...) (cdr val))
       (case val rest ...))]))

; Switch
(define-syntax switch
  (syntax-rules (else)
    ; Else
    [(_ val [else e1 e2 ...]) (begin e1 e2 ...)]
    ; No cases left
    [(_ val) (error "Switch" "No match found" val)]
    ; Cases left
    [(_ val [compare e1 e2 ...] rest ...)
     (if (eq? val compare)
         (begin e1 e2 ...)
       (switch val rest ...))]))

; Constant functions
(define-syntax-rule (const b)
  (lambda () b))
