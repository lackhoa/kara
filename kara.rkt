#lang racket

(require "kara_eval.rkt")
(provide kload)
(provide repl)

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
                    (close-input-port input)
                    (display "Loading done.") (newline))
                (begin
                    (let ((interpreted (interpret next)))
                        (when
                            (not (or (eq? interpreted (void))
                                     (eq? interpreted 'ok)))
                            (display-output interpreted)))
                    (reader (read input)))))))

; -----------------------------------------------------------
; Built-in stuff
; -----------------------------------------------------------
; The initial frame: contain compound masks for primitive...
; procedures so that we can have fun with keyword bindings.
; Not actually needed since we already have PRIMITIVE_TAG,...
; but it doesn't hurt to populate the initial frame with useful things.
; Let me tell you what's going on: the list to the right of the...
; '!' needs to be quasiquoted because we want to evaluate the...
; variable inside, but before the variable we need to quote because...
; we don't want to execute the data to be fed to the primitive function.
(define The-frame (new-frame))
; List
(update-frame! The-frame 'car '(! '(car ',,$0)))
(update-frame! The-frame 'cdr '(! '(cdr ',,$0)))
(update-frame! The-frame 'cons '(! '(cons ',,$0 ',,$1)))
(update-frame! The-frame 'null? '(! '(null? ',,$0)))
; Arithmetic
(update-frame! The-frame 'not '(if $0 #f #t))
(update-frame! The-frame '+ '(! '(+ ',,$0 ',,$1)))
(update-frame! The-frame '- '(! '(- ',,$0 ',,$1)))
(update-frame! The-frame '* '(! '(* ',,$0 ',,$1)))
(update-frame! The-frame '/ '(! '(/ ',,$0 ',,$1)))
(update-frame! The-frame '< '(! '(< ',,$0 ',,$1)))
(update-frame! The-frame '= '(! '(= ',,$0 ',,$1)))
(update-frame! The-frame 'remainder '(! '(remainder ',,$0 ',,$1)))
(update-frame! The-frame 'even? '(! '(even? ',,$0)))
(update-frame! The-frame 'random '(! '(random ',,$0)))
; Hashtable
(update-frame! The-frame 'make-hash '(! '('make-hash)))
(update-frame! The-frame 'hash-has-key? '(! '(hash-has-key? ',,$0 ',,$1)))
(update-frame! The-frame 'hash-ref '(! '(hash-ref ',,$0 ',,$1 ',,$2)))
(update-frame! The-frame 'hash-set! '(! '(hash-set! ',,$0 ',,$1 ',,$2)))
(update-frame! The-frame 'hash-keys '(! '(hash-keys ',,$0)))
; System
(update-frame! The-frame 'raise '(! '(raise ',,$0)))
(update-frame! The-frame 'error '(! '(error ',,$0 ',,$1 ',,$2)))
(update-frame! The-frame 'void '(! '(void)))
; I/O
(update-frame! The-frame 'newline '(! '(newline)))
(update-frame! The-frame 'display '(! '(display ',,$0 ',,$1)))
(update-frame! The-frame 'load '(! '(kload ',,$0)))


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

(define input-prompt "K<<< ")

(define output-prompt "K>>> ")

(define (repl)
    (display input-prompt)
    (let ((input (read)))
        (display-output (interpret input)))
    (repl))

(define (display-output output)
    (display output-prompt) (display output) (newline))