(load "kara_eval.scm")

(trace analyze eval)

; ------------------------------------------------------------
; The interpreter
; ------------------------------------------------------------
(define (interpret exp) (eval exp global-env))

(define (parse input-file-name)
    (let ([input (open-input-file input-file-name)])
        ; This guy uses named let
        (let reader ([next (read input)])
            (if (eof-object? next)
                (begin
                    (close-port input)
                    (display "Done!"))
                (begin
                    (display (interpret next))
                    (newline)
                    (reader (read input)))))))

; ------------------------------------------------------------
; The Repl
; ------------------------------------------------------------

(define input-prompt "K>>> ")

(define (driver-loop)
    (display input-prompt)
    (let ((input (read)))
        (display (interpret input)))
    (newline)
    (driver-loop))
