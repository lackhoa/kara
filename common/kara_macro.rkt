#lang racket

(provide lam def case const)


; "lam" instead of "lambda"
(define-syntax-rule (lam whatever ...)
  (lambda whatever ...))

; "def" instead of "define"
(define-syntax-rule (def whatever ...)
  (define whatever ...))

; Pattern matching
(define-syntax case
  (syntax-rules ()
    ; No cases left
    [(_ val)
       (error "Pattern matching" "Pattern matching failed" val)]

    [(_ val
       [(pat-con1 var1 ...) b1-first b1-rest ...]  ; At least one body
       [(pat-con2 var2 ...) b2-first b2-rest ...] ...)
     ; I wonder how the quotation on `pat-con1` works?
     (if (eq? (car val) 'pat-con1)
         (apply (lambda (var1 ...) b1-first b1-rest ...) (cdr val))
           (case val
             [(pat-con2 var2 ...) b2-first b2-rest ...] ...))]))

; Constant functions
(define-syntax-rule (const b)
  (lambda () b))



