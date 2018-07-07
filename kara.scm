(define
  (eval exp env)
  (cond
    ((self-eval? exp) exp)
    ((quoted? exp) (quoted-text exp))
    ((var? exp) (lookup-var exp env))
    ((asgn? exp) (eval-asgn exp env))
    ((if? exp) (eval-if exp env))
    ((cond? exp) (eval (cond->if exp) env))
    (
      (lambda? exp)
      (make-proc (lambda-body exp) env)  ; No lambda parameters
    )
    ((seq? exp) (eval-seq (begin-actions exp) env))
    ((pair? exp) (eval-apply exp))
    (else (error "eval" "Unknown expression type" exp))
  )
)

; Types of expressions and their structures
(define (tagged? exp tag) (and  (pair? exp)
                                (eq? (car exp) tag)))

(define
  (self-eval? exp)
  (or (number? exp) (string? exp) (eq? exp '#t) (eq? exp '#f))
)

(define (var? exp) (symbol? exp))

(define (asgn? exp) (tagged? exp 'set!))
(define (asgn-binding exp) (cdr exp))

(define (quoted? exp) (tagged? exp 'quote))
(define (quoted-text exp) (cadr exp))

(define (if? exp) (tagged? exp 'if))
(define (if-pred exp) (cadr exp))
(define (if-conse exp) (caddr exp))
(define
  (if-alt exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false
  )
)

(define (cond? exp) (tagged? exp 'cond))
(define (cond-clauses cond-exp) (cdr cond-exp))
(define (cond-else-clause clause) (eq? (car clause) 'else))
(define (cond-pred clause) (car clause))
(define (cond-actions clause) (cadr clause))

(define (seq? exp) (tagged? exp 'seq))
(define (seq-body exp) (cdr exp))

(define (application-proc exp) (car exp))
(define (application-frame-init exp) (car exp))

(define (keyword? exp) (tagged? exp '**))


; The environment
; Binding: a pair of variable and value
(define (binding-var binding) (car binding))
(define (binding-val binding) (cadr binding))

; Frame: a hash table to store bindings
(define (make-frame) (make-eq-hashtable))

(define
  (update-frame! frame binding)
  (hashtable-set! frame (binding-var binding) (binding-val binding))
)

(define
  (bound-in-frame? frame var)
  (hashtable-contains? frame var)
)

(define
  (frame-lookup frame var)
  (if
    (bound-in-frame? frame var)
    (hashtable-ref frame var '())
    (error "frame-lookup" "Binding not in frame" binding)
  )
)

; Environment: a list of frames
; binginning with the inner most frame to the outermost
; environments are immutable, but frames can change
(define (inner-frame env) (car env))
(define (outer-frames env) (cdr env))
(define empty-env '())
(define (extended-env env frame) (cons frame env))

; Variable lookup: lookup through the entire environment
(define
  (lookup-var var env)
  (cond ((eq? env empty-env) (error "lookup-var" "Unbound variable" var))
        (
          (bound-in-frame? (inner-frame env) var)
          (frame-lookup (inner-frame env) var)
        )
        (lookup-var var (outer-frames env))
  )
)

; Assignment: can only change the inner frame
(define
  (eval-asgn assignment env)
  (update-frame!
    (inner-frame env)
    (asgn-binding assignment)
  )
  'ok
)


; The conditionals
(define (eval-if exp env)
  (if (eval (if-pred exp) env)
      (eval (if-conse exp) env)
      (eval (if-alt exp) env)
  )
)

(define
  (expand-clauses clauses)
  (let
    ((cur-clause (car clauses)))
    (if (cond-else-clause cur-clause)
        (if (null? (cdr clauses))
            (cond-actions cur-clause)
            (error "cond->if" "ELSE clause isn't last" clauses)
        )
        (list 'if
              (cond-pred cur-clause)
              (cond-actions cur-clause)
              (expand-clauses (cdr clauses))
        )
    )
  )
)

(define (cond->if exp)
        (expand-clauses (cond-clauses exp))
)


; Function application
; The code is evaluated in the enclosing environment
; While the execution is done in a new local environment
(define
  (eval-apply exp env)
  (
    (eval (application-proc exp) env)  
    (fork-env (application-frame-init exp) env)
  )
)


(define
  (frame-init->frame frame-init)
  (cond
    ((null? frame-init) '())
    (
      (keyword? (binding-exp))
      (let
        (
          (binding-exp (car frame-init))
          (more-bindings (frame-init->frame (cdr frame-init)))
        )
        ( (cons (binding-exp-binding (binding-exp)) more-bindings) )
      )
    )
    (
      else
      (let
        (
          (more-bindings (frame-init->frame (cdr frame-init)))
        )
        ( (cons  more-bindings) )
      )
    )
  )
)

(define
  (fork-env frame-init base-env)
  (extend-env (frame-init->frame frame-init) base-env)
)


; The Repl
(define global-env (list (make-eq-hashtable)))

(define input-prompt "K>>> ")

(define output-prompt "; ; ;  M-Eval value:")

(define
  (driver-loop)
  (prompt-for-input)
  (print-output (eval input global-env))
  (driver-loop)
)

(define (prompt-for-input) (newline) (display input-prompt))

(define (print-output obj)
  (display obj)
)

; The program to run
(trace eval expand-clauses)
(display "Setting a variable\n\n")
(eval '(set! a 5) global-env)
(display "Retrieving a variable\n\n")
(eval 'a global-env)
(display "If true\n\n")
(eval '(if #t 1 2) global-env)
(display "If false\n\n")
(eval '(if #f 1 2) global-env)
(display "Conditional first\n\n")
(eval '(cond (#t 1) (#f 2) (else 3)) global-env)
(display "Conditional second\n\n")
(eval '(cond (#f 1) (#t 2) (else 3)) global-env)
(display "Conditional else\n\n")
(eval '(cond (#f 1) (#f 2) (else 3)) global-env)



