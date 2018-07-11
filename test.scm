(load "kara.scm")

(trace eval analyze analyze-exec)

(define (my-display str)
  (newline) (newline) (display str) (newline) (newline))

; (my-display "Setting and retrieving a variable")
; (eval '(set! a 5) global-env)
; (eval 'a global-env)

; (my-display "If true")
; (eval '(if #t 1 2) global-env)

; (my-display "If false")
; (eval '(if #f 1 2) global-env)

; (my-display "Conditional first")
; (eval '(cond (#t 1) (#f 2) (else 3)) global-env)

; (my-display "Conditional second")
; (eval '(cond (#f 1) (#t 2) (else 3)) global-env)

; (my-display "Conditional else")
; (eval '(cond (#f 1) (#f 2) (else 3)) global-env)

; (my-display "Apply complex primitive procedure")
; (eval '(+ (+ 2 8) 2) global-env)

; (my-display "Apply compound procedure 1")
; (eval '(''z) global-env)

; (my-display "Apply compound procedure 2")
; (eval '('z (** z 6)) global-env)

; (my-display "Apply compound procedure 3")
; (eval '('(+ x y) (** x 7) (** y 8)) global-env)

; (my-display "Apply compound procedure 4")
; (eval '('$0 7) global-env)

; (my-display "Apply compound procedure 5")
; (eval '('(+ $0 $1) 7 13) global-env)

; (my-display "Both keyword and non-keyword")
; (eval '('(* mult $0) 7 (** mult 8)) global-env)

; (my-display "Store procedure and use them later")
; (eval '(set! add3 '(+ 3 $0)) global-env)
; (eval '(add3 7) global-env)

; (my-display "map procedure")
; (eval
;   '(set! map '(if (null? L) '() (cons (func (car L)) (map (** L (cdr L)) (** func func)))))
;   global-env)
; (eval '(map (** func add3) (** L (list 0 1 2 3 4 5))) global-env)

; (my-display "Currying")
; (eval '(set! partial (quote (subtract (** $0 9)))) global-env)
; (eval '(partial (** $1 27)) global-env)

; (my-display "Over-evaluation behavior")
; (eval '((+ 3 4)) global-env)

; (my-display "Sequencing")
; (eval '(seq (set! x 9) (* x 4)) global-env)

