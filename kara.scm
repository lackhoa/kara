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
(define The-frame (new-frame))
; List
(update-frame! The-frame 'car '`(!p (car ,$0)))
(update-frame! The-frame 'cdr '`(!p (cdr ,$0)))
(update-frame! The-frame 'set-car! '`(!p (set-car! ,$0 ,$1)))
(update-frame! The-frame 'cons '`(!p (cons ,$0 ,$1)))
(update-frame! The-frame 'cons '`(!p (null? ,$0)))
; Arithmetic
(update-frame! The-frame '+ '`(!p (+ ,$0 ,$1)))
(update-frame! The-frame '- '`(!p (- ,$0 ,$1)))
(update-frame! The-frame '* '`(!p (* ,$0 ,$1)))
(update-frame! The-frame '/ '`(!p (/ ,$0 ,$1)))
(update-frame! The-frame '< '`(!p (< ,$0 ,$1)))
(update-frame! The-frame '= '`(!p (= ,$0 ,$1)))
(update-frame! The-frame 'remainder '`(!p (remainder ,$0 ,$1)))
(update-frame! The-frame 'even? '`(!p (even? ,$0 ,$1)))
(update-frame! The-frame 'random '`(!p (random)))
; Hashtable
(update-frame! The-frame 'make-eq-hashtable '`(!p (make-eq-hashtable)))
(update-frame! The-frame 'hashtable-contains? '`(!p (hashtable-contains? ,$0 ,$1)))
(update-frame! The-frame 'hashtable-ref '`(!p (hashtable-ref ,$0 ,$1 ,$2)))
(update-frame! The-frame 'hashtable-set! '`(!p (hashtable-set! ,$0 ,$1 ,$2)))
(update-frame! The-frame 'hashtable-keys '`(!p (hashtable-keys ,$0)))
; System
(update-frame! The-frame 'raise '`(!p (raise ,$0)))
(update-frame! The-frame 'error '`(!p (error ,$0 ,$1 ,$2)))
(update-frame! The-frame 'void '`(!p (void)))
; I/O
(update-frame! The-frame 'newline '`(!p (newline)))
(update-frame! The-frame 'display '`(!p (display ,$0 ,$1)))
(update-frame! The-frame 'load '`(!p (kload ,$0)))


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

(trace keval analyze analyze-quasiquoted)
