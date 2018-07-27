#lang racket

(provide lam
         def
         case)


; "lam" instead of "lambda"
(define-syntax-rule (lam x ...)
  (lambda x ...))

; "def" instead of "define"
(define-syntax-rule (def x ...)
  (define x ...))

; Pattern matching
(define-syntax case
  (syntax-rules ()
    ; No cases left
    [(case val)
       (error "Pattern matching" "Pattern matching failed" val)]

    [(case val
       [(pat-con1 var1 ...) action1 ...]
       [(pat-con2 var2 ...) action2 ...] ...)
     ; I wonder how the quotation on `pat-con1` works?
     (if (eq? (car val) 'pat-con1)
         (let ([var1 (cadr val)] ...)
           (begin action1 ...))
           (case val
             [(pat-con2 var2 ...) action2 ...] ...))]))

