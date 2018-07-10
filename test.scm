(load "kara.scm")

(trace eval eval-exec env-lookup frame-lookup)
; (display "\n\nSetting a variable\n\n")
; (eval '(set! a 5) global-env)

; (display "\n\nRetrieving a variable\n\n")
; (eval 'a global-env)

; (display "\n\nIf true\n\n")
; (eval '(if #t 1 2) global-env)

; (display "\n\nIf false\n\n")
; (eval '(if #f 1 2) global-env)

; (display "\n\nConditional first\n\n")
; (eval '(cond (#t 1) (#f 2) (else 3)) global-env)

; (display "\n\nConditional second\n\n")
; (eval '(cond (#f 1) (#t 2) (else 3)) global-env)

; (display "\n\nConditional else\n\n")
; (eval '(cond (#f 1) (#f 2) (else 3)) global-env)

; (display "\n\nApply complex primitive procedure\n\n")
; (eval '(+ (+ 2 8) 2) global-env)

; (display "\n\nApply compound procedure 1\n\n")
; (eval '(''z) global-env)

; (display "\n\nApply compound procedure 2\n\n")
; (eval '('z (** z 6)) global-env)

; (display "\n\nApply compound procedure 3\n\n")
; (eval '('(+ x y) (** x 7) (** y 8)) global-env)

; (display "\n\nApply compound procedure 4\n\n")
; (eval '('$0 7) global-env)

; (display "\n\nApply compound procedure 5\n\n")
; (eval '('(+ $0 $1) 7 13) global-env)

; (display "\n\nBoth keyword and non-keyword\n\n")
; (eval '('(* mult $0) 7 (** mult 7)) global-env)

; (display "\n\nStore procedure\n\n")
; (eval '(set! add3 '(+ 3 $0)) global-env)

; (display "\n\nUse stored procedure\n\n")
; (eval '(add3 7) global-env)

; (newline)(newline)
; (eval
;   '(set! map '(if (null? L) '() (cons (func (car L)) (map (** L (cdr L)) (** func func)))))
;   global-env)

; (display "\n\nmap procedure\n\n")
; (eval '(map (** func add3) (** L (list 0 1 2 3 4 5))) global-env)

(display "\n\nCurrying\n\n")
(eval '(set! partial (quote (subtract (** $0 9)))) global-env)
(eval '(partial (** $1 27)) global-env)



