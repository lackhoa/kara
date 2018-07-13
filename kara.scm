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
; The primitive procedures and their representation
(define prim-proc-table (make-eq-hashtable))
; List
(hashtable-set! prim-proc-table 'car car)
(hashtable-set! prim-proc-table 'cdr cdr)
(hashtable-set! prim-proc-table 'set-car! set-car!)
(hashtable-set! prim-proc-table 'cons cons)
(hashtable-set! prim-proc-table 'list list)
(hashtable-set! prim-proc-table 'null? null?)
; Arithmetic
(hashtable-set! prim-proc-table '+ +)
(hashtable-set! prim-proc-table '- -)
(hashtable-set! prim-proc-table '* *)
(hashtable-set! prim-proc-table '/ /)
(hashtable-set! prim-proc-table '> >)
(hashtable-set! prim-proc-table '< <)
(hashtable-set! prim-proc-table '= =)
(hashtable-set! prim-proc-table 'even? even?)
(hashtable-set! prim-proc-table 'odd? odd?)
(hashtable-set! prim-proc-table 'remainder remainder)
(hashtable-set! prim-proc-table 'random random)
; Hashtable
(hashtable-set! prim-proc-table 'make-eq-hashtable make-eq-hashtable)
(hashtable-set! prim-proc-table 'hashtable-contains? hashtable-contains?)
(hashtable-set! prim-proc-table 'hashtable-set! hashtable-set!)
(hashtable-set! prim-proc-table 'hashtable-ref hashtable-ref)
(hashtable-set! prim-proc-table 'hashtable-keys hashtable-keys)
; System
(hashtable-set! prim-proc-table 'raise raise)
(hashtable-set! prim-proc-table 'error error)
(hashtable-set! prim-proc-table 'void void)
; I/O
(hashtable-set! prim-proc-table 'newline newline)
(hashtable-set! prim-proc-table 'display display)
(hashtable-set! prim-proc-table 'open-file-output-port open-file-output-port)
(hashtable-set! prim-proc-table 'load kload)  ; Written right below

; The initial frame: contain compound masks for primitive procedures...
; so that we can supply keyword bindings.
(define The-frame (new-frame))
(update-frame! The-frame 'add '(+ $0 $1))
(update-frame! The-frame 'mult '(* $0 $1))
(update-frame! The-frame 'subtract '(- $0 $1))
(update-frame! The-frame 'and '(and $0 $1))
(update-frame! The-frame 'or '(or $0 $1))

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
