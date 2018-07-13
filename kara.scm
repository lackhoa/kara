(load "kara_eval.scm")

; ------------------------------------------------------------
; The interpreter
; ------------------------------------------------------------
(define (interpret exp) (keval exp global-env))

(define (parse input-file-name)
    (let ([input (open-input-file input-file-name)])
        ; This guy uses named let
        (let reader ([next (read input)])
            (if (eof-object? next)
                (begin
                    (close-port input)
                    (display "Parsing All Done!"))
                (begin
                    (let ((in (interpret next)))
                        (if (not (eq? in (void))) (begin (display in) (newline))))
                    (reader (read input)))))))

; ------------------------------------------------------------
; The Repl
; ------------------------------------------------------------

(define input-prompt "K>>> ")

(define (repl)
    (display input-prompt)
    (let ((input (read)))
        (display (interpret input)))
    (newline)
    (repl))
