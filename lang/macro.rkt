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

; Switch: an even simpler dispatch than `case`
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

; Add an element to the beginning of the list.
(define-syntax-rule (cons! item ls)
  (set! ls (cons item
                 ls)))

; Same, but with streams
(define-syntax-rule (stream-cons! item stream)
  (set! stream (stream-cons item
                            stream)))
