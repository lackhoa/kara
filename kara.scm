; The main switch block
(define (eval exp env)
  (cond
    ((self-eval? exp) exp)
    ((quoted? exp) (quoted-text exp))
    ((var? exp) (env-lookup exp env))
    ((asgn? exp) (eval-asgn exp env))
    ((if? exp) (eval-if exp env))
    ((cond? exp) (eval (cond->if exp) env))
    ((seq? exp) (eval-seq (seq-actions exp) env))
    (
      (lambda? exp)
      (lambda () (eval-seq (lambda-body exp) env))  ; No lambda parameters
    )
    ((pair? exp) (eval-apply exp env))
    (else (error "eval" "Unknown expression type" exp))
  )
)

; Types of expressions and their structures
(define (tagged? exp tag) (and  (pair? exp)
                                (eq? (car exp) tag)))

(define (self-eval? exp)
  (or (number? exp) (string? exp) (eq? exp #t) (eq? exp #f))
)

(define (var? exp) (symbol? exp))

(define (asgn? exp) (tagged? exp 'set!))
(define (asgn-binding exp) (cdr exp))

(define (quoted? exp) (tagged? exp 'quote))
(define (quoted-text exp) (cadr exp))

(define (if? exp) (tagged? exp 'if))
(define (if-pred exp) (cadr exp))
(define (if-conse exp) (caddr exp))
(define (if-alt exp)
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
(define (seq-actions exp) (cdr exp))

; Code execution is split in categories:
; Primitive procedure _application_, and
; Compound procedure _call_
(define (call-proc exp) (car exp))
(define (call-frame-init exp) (cdr exp))
(define (application-proc exp) (car exp))
(define (application-args exp) (cdr exp))

(define (lambda? exp) (tagged? exp 'lambda))
(define (lambda-body exp) (cadr exp))  ;Note that lambda's body is NOT an implicit sequence


; The environment
; Binding: a pair of variable and value
(define (binding-var binding) (car binding))
(define (binding-val binding) (cadr binding))

; Frame: a hash table to store bindings
; with an additional cell to store unnamed variables
(define (make-frame)
  (let ((result (make-eq-hashtable)))
    (hashtable-set! result '$ '())
    result
  )
)

(define (update-frame! frame binding)
  (hashtable-set! frame (binding-var binding) (binding-val binding))
)

(define (bound-in-frame? frame var)
  (if (unnamed-var? var)
      (if (hashtable-contains? frame '$)
          (> (length (frame-lookup frame '$)) (unnamed-var->int var))
          (error "bound-in-frame?"
                 "Frame does not contained unnamed slots"
                 frame
          )
      )
      (hashtable-contains? frame var)  ; Named variable
  )
)

(define (unnamed-var? var)
        (eq? (string-ref (symbol->string var) 0) '#\$)
)

(define (unnamed-var->int var)
  (cond
    ((eq? var '$0) 0) ((eq? var '$1) 1) ((eq? var '$2) 2)
    ((eq? var '$3) 3) ((eq? var '$4) 4) ((eq? var '$5) 5)
    ((eq? var '$6) 6) ((eq? var '$7) 7) ((eq? var '$8) 8)
    ((eq? var '$9) 9)
    (else (error "unnamed-var->int" "Invalid unnamed variable" var))
  )
)

(define (frame-lookup frame var)
  (if (bound-in-frame? frame var)
      (if (unnamed-var? var)
          (list-ref (frame-lookup '$) (unnamed-var->int var))
          (hashtable-ref frame var '())
      )
      (error "frame-lookup" "Binding not in frame" binding)
  )
)

;Add an anonymous variable to a frame
(define (add-unnamed-to-frame! frame value)
  (let
    ((unnamed-vars (hashtable-ref frame $ '())))
    (hashtable-set! frame '$ (append unnamed-vars '(value)))
  )
)

; Environment: a list of frames
; binginning with the local frame to the outermost
; environments are immutable, but frames can change
(define (local-frame env) (car env))
(define (outer-frames env) (cdr env))
(define empty-env '())
(define (extend-env env frame) (cons frame env))

; Variable lookup through the entire environment
; Unnamed variables are only looked up in the local frame
(define (env-lookup var env)
  (cond ((eq? env empty-env) (error "env-lookup" "Unbound named variable" var))
        (
          (bound-in-frame? (local-frame env) var)
          (frame-lookup (local-frame env) var)
        )
        (else
          (if (unnamed-var? var)
              (error "env-lookup" "Unbound unnamed variable" var)
              (env-lookup var (outer-frames env))
          )
        )
  )
)

; Assignment: can only change the local frame
(define (eval-asgn assignment env)
  (update-frame!
    (local-frame env)
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

(define (expand-clauses clauses)
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


; Evaluate a sequence
(define (eval-seq sequence env)
  (eval (car sequence) env)
  (if (not (null? (cdr sequence)))
      (eval-seq (cdr sequence) env)
  )
)

(define (eval-many exps env)
  (if (null? exps)
      '()
      (cons (eval (car exps) env)
            (eval-many (cdr exps) env)
      )
  )
)

; Function application
; The code is evaluated in the enclosing environment
; While the execution is done in a new local environment
(define (eval-apply exp env)
  (if
    (prim-proc? (car exp))
    ; Primitive
    (apply-prim-proc (application-proc exp)
                     (eval-many (application-args exp) env)
    )
    ; Compound
    (eval (call-proc exp)
          (fork-env (call-frame-init exp) env)
    )
  )
)

(define (frame-init->frame frame-init frame)
  (define (build-frame frame-init frame)
    (if (not (null? frame-init))
        (begin
          (let ((first (car frame-init)) (rest (cdr frame-init)))
            (if (tagged? first '**)
                ; Bindings
                (update-frame! frame (cdr first))
                ; Unnamed variable
                (add-unnamed-to-frame! frame first)
            )
            (build-frame rest frame)
          )
        )
    )
  )

  (build-frame frame-init (make-frame))
)

(define (fork-env frame-init base-env)
  (extend-env base-env (frame-init->frame frame-init))
)


; The Repl
(define prim-procs
  (let ((result (make-eq-hashtable)))
    (hashtable-set! result 'car car)
    (hashtable-set! result 'cdr cdr)
    (hashtable-set! result 'list list)
    (hashtable-set! result 'null? null?)
    (hashtable-set! result 'pair? pair?)
    (hashtable-set! result '+ +)
    (hashtable-set! result '- -)
    (hashtable-set! result '* *)
    (hashtable-set! result '> >)
    (hashtable-set! result '< <)

    result
  )
)

(define (prim-proc? proc) (hashtable-contains? prim-procs proc))

; Vanilla, primitive Scheme application
(define (apply-prim-proc proc args)
  (apply (hashtable-ref prim-procs proc (void))
         args
  )
)

(define (make-the-frame)
  (set! the-frame (make-frame))
  (update-frame! the-frame (list 'car car))
  (update-frame! the-frame (list 'cdr cdr))
  (update-frame! the-frame (list 'cons cons))
  (update-frame! the-frame (list '+ +))
  the-frame
)
(define global-env (list (make-the-frame)))

(define input-prompt "K>>> ")

(define output-prompt "; ; ;  M-Eval value:")

(define (driver-loop)
  (prompt-for-input)
  (let ((input (read)))
    (print-output (eval input global-env))
  )
  (driver-loop)
)

(define (prompt-for-input) (newline) (display input-prompt))

(define (print-output obj)
  (display obj)
)

; The program to run
(trace eval eval-apply)
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
(display "Apply complex primitive procedure\n\n")
(eval '(+ (+ 2 8) 2) global-env)
(display "Apply compound procedure unnamed variables\n\n")
(eval '((lambda ($1)) 2) global-env)






