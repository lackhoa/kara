(load "kara_eval.scm")

; ------------------------------------------------------------
; The interpreter
; ------------------------------------------------------------
(define (interpret exp) (keval exp global-env))

(define (kload input-file-name)
    (let ([input (open-input-file input-file-name)])
        ; This guy uses named let
        (let reader ([next (read input)])
            (if (eof-object? next)
                (begin
                    (close-port input)
                    (display "Loading all done.") (newline))
                (begin
                    (let ((in (interpret next)))
                        (if (not (eq? in (void))) (begin (display in) (newline))))
                    (reader (read input)))))))

; -----------------------------------------------------------
; Built-in stuff
; -----------------------------------------------------------
; The initial frame: contain compound masks for primitive...
; procedures so that we can have fun with keyword bindings.
; Not actually needed since we already have PRIMITIVE_TAG,...
; but it doesn't hurt to populate the initial frame with useful things.
; Let me tell you what's going on: the list to the right of the...
; `!` needs to be quasiquoted because we want to evaluate the...
; variable inside, but before the variable we need to quote because...
; we don't want to execute the data to be fed to the primitive function.
(define The-frame (new-frame))
; List
(update-frame! The-frame 'car '(! `(car ',$0)))
(update-frame! The-frame 'cdr '(! `(cdr ',$0)))
(update-frame! The-frame 'set-car! '(! `(set-car! ',$0 ',$1)))
(update-frame! The-frame 'cons '(! `(cons ',$0 ',$1)))
(update-frame! The-frame 'null? '(! `(null? ',$0)))
; Arithmetic
(update-frame! The-frame '+ '(! `(+ ',$0 ',$1)))
(update-frame! The-frame '- '(! `(- ',$0 ',$1)))
(update-frame! The-frame '* '(! `(* ',$0 ',$1)))
(update-frame! The-frame '/ '(! `(/ ',$0 ',$1)))
(update-frame! The-frame '< '(! `(< ',$0 ',$1)))
(update-frame! The-frame '= '(! `(= ',$0 ',$1)))
(update-frame! The-frame 'remainder '(! `(remainder ',$0 ',$1)))
(update-frame! The-frame 'even? '(! `(even? ',$0 ',$1)))
(update-frame! The-frame 'random '(! `(random)))
; Hashtable
(update-frame! The-frame 'make-eq-hashtable '(! `(make-eq-hashtable)))
(update-frame! The-frame 'hashtable-contains? '(! `(hashtable-contains? ',$0 ',$1)))
(update-frame! The-frame 'hashtable-ref '(! `(hashtable-ref ',$0 ',$1 ',$2)))
(update-frame! The-frame 'hashtable-set! '(! `(hashtable-set! ',$0 ',$1 ',$2)))
(update-frame! The-frame 'hashtable-keys '(! `(hashtable-keys ',$0)))
; System
(update-frame! The-frame 'raise '(! `(raise ',$0)))
(update-frame! The-frame 'error '(! `(error ',$0 ',$1 ',$2)))
(update-frame! The-frame 'void '(! `(void)))
; I/O
(update-frame! The-frame 'newline '(! `(newline)))
(update-frame! The-frame 'display '(! `(display ',$0 ',$1)))
(update-frame! The-frame 'load '(! `(kload ',$0)))


; The global environment with The frame
(define global-env (list The-frame))

; ------------------------------------------------------------
; The Common Library written in Kara
; ------------------------------------------------------------
(display "Loading the Common Library...") (newline)
(kload "common.kar")

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
