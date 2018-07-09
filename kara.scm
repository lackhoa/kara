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
    ((pair? exp) (eval-exec exp env))
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
(define (asgn-var exp) (cadr exp))
(define (asgn-val exp) (caddr exp))

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

(define (update-frame! frame var val)
  (hashtable-set! frame var val)
)

(define (bound-in-frame? frame var)
  (if (unnamed-var? var)
      (if (hashtable-contains? frame '$)
          (> (length (hashtable-ref frame '$ (void))) (unnamed-var->int var))
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
          (list-ref (hashtable-ref frame '$ (void))
                    (unnamed-var->int var)
          )
          (hashtable-ref frame var (void))
      )
      (error "frame-lookup" "Binding not in frame" binding)
  )
)

;Add an anonymous variable to a frame
(define (add-unnamed-to-frame! frame value)
  (if (hashtable-contains? frame '$)
      (let
        ((unnamed-vars (hashtable-ref frame '$ (void))))
        (hashtable-set! frame '$ (append unnamed-vars (list value)))
      )
      (error "add-unnamed-to-frame" "Frame has no slots for unnamed variables" frame)
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
  (cond ((eq? env empty-env) (error "env-lookup" "Unbound variable" var))
        (
          (bound-in-frame? (local-frame env) var)
          (frame-lookup (local-frame env) var)
        )
        (else
          (if (unnamed-var? var)
              (error "env-lookup" "Unbound variable" var)
              (env-lookup var (outer-frames env))
          )
        )
  )
)

; Assignment: can only change the local frame
; The value must be immediately evaluated in the current environment
(define (eval-asgn assignment env)
  (update-frame!
    (local-frame env)
    (asgn-var assignment)
    (eval (asgn-val assignment) env)
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
(define (eval-exec exp env)
  (if
    (prim-proc? (car exp))
    ; Primitive
    (apply-prim-proc (application-proc exp)
                     (eval-many (application-args exp) env)
    )
    ; Compound
    (eval
      (eval (call-proc exp) env)
      (fork-env (call-frame-init exp) env)
    )
  )
)

(define (frame-init->frame frame-init env)
  (define (build-frame frame-init frame)
    (if (null? frame-init)
        frame
        (begin
          (let ((first (car frame-init)) (rest (cdr frame-init)))
            (if (tagged? first '**)
                ; Named variable
                (update-frame! frame (cadr first) (eval (caddr first) env))
                ; Unnamed variable
                (add-unnamed-to-frame! frame (eval first env))
            )
            (build-frame rest frame)
          )
        )
    )
  )

  (build-frame frame-init (make-frame))
)

(define (fork-env frame-init base-env)
  (extend-env base-env (frame-init->frame frame-init base-env))
)


; The Repl
(define prim-procs
  (let ((result (make-eq-hashtable)))
    (hashtable-set! result 'car car)
    (hashtable-set! result 'cdr cdr)
    (hashtable-set! result 'cons cons)
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

; The global environment with an (almost) empty frame
(define global-env (list (make-frame)))

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
(trace eval eval-exec)
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
; (eval '('z) global-env)

; (display "\n\nApply compound procedure 2\n\n")
; (eval '(z (** z 6)) global-env)

; (display "\n\nApply compound procedure 3\n\n")
; (eval '((+ x y) (** x 7) (** y 8)) global-env)

; (display "\n\nApply compound procedure 4\n\n")
; (eval '($0 7) global-env)

; (display "\n\nApply compound procedure 5\n\n")
; (eval '((+ $0 $1) 7 13) global-env)

; (display "\n\nBoth keyword and non-keyword\n\n")
; (eval '((* mult $0) 7 (** mult 7)) global-env)

(display "\n\nStore procedure\n\n")
(eval '(set! add3 '(+ 3 $0)) global-env)

(display "\n\nUse stored procedure\n\n")
(eval '(add3 7) global-env)

(newline)(newline)
(eval
  '(set! map '(if (null? L) '() (cons (func (car L)) (map (** L (cdr L)) (** func func)))))
  global-env)

(display "\n\nmap procedure\n\n")
(eval '(map (** func add3) (** L (list 0 1 2 3 4 5))) global-env)











