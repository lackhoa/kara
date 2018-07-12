(load "kara.scm")

(trace interpret analyze analyze-exec)

(define (my-display str)
  (newline) (newline) (display str) (newline) (newline))

; (my-display "Setting and retrieving a variable")
; (interpret '(set! a 5))
; (interpret 'a)

; (my-display "If true")
; (interpret '(if #t 1 2))

; (my-display "If false")
; (interpret '(if #f 1 2))

; (my-display "Conditional first")
; (interpret '(cond (#t 1) (#f 2) (else 3)))

; (my-display "Conditional second")
; (interpret '(cond (#f 1) (#t 2) (else 3)))

; (my-display "Conditional else")
; (interpret '(cond (#f 1) (#f 2) (else 3)))

; (my-display "Apply complex primitive procedure")
; (interpret '(+ (+ 2 8) 2))

; (my-display "Apply compound procedure 1")
; (interpret '(''z))

; (my-display "Apply compound procedure 2")
; (interpret '('z (** z 6)))

; (my-display "Apply compound procedure 3")
; (interpret '('(+ x y) (** x 7) (** y 8)))

; (my-display "Apply compound procedure 4")
; (interpret '('$0 7))

; (my-display "Apply compound procedure 5")
; (interpret '('(+ $0 $1) 7 13))

; (my-display "Both keyword and non-keyword")
; (interpret '('(* mult $0) 7 (** mult 8)))

; (my-display "Store procedure and use them later")
; (interpret '(set! add3 '(+ 3 $0)))
; (interpret '(add3 7))

; (my-display "map procedure")
; (interpret
;   '(set! map '(if (null? L) '() (cons (func (car L)) (map (** L (cdr L)) (** func func)))))
;  )
; (interpret '(map (** func add3) (** L (list 0 1 2 3 4 5))))

; (my-display "Currying")
; (interpret '(set! partial (quote (subtract (** $0 9)))))
; (interpret '(partial (** $1 27)))

; (my-display "Over-interpretuation behavior")
; (interpret '((+ 3 4)))

; (my-display "Sequencing")
; (interpret '(seq (set! x 9) (* x 4)))

(my-display "Factorial")
(interpret '(set! fact '(if (= $0 1) 1 (* (fact (- $0 1)) $0))))
(interpret '(fact 3))
