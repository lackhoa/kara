;;; The main switch block
(define (eval exp env)
  (cond
    ((self-eval? exp) exp)
    ((quoted? exp) (quoted-text exp))
    ((var? exp) (env-lookup exp env))
    ((asgn? exp) (eval-asgn exp env))
    ((if? exp) (eval-if exp env))
    ((cond? exp) (eval (cond->if exp) env))
    ((seq? exp) (eval-seq (seq-actions exp) env))
    ((pair? exp) (eval-exec exp env))  ; Code execution is last
    (else (error "eval" "Unknown expression type" exp))
  )
)

; -------------------------------------------------------------
; Types of expressions and their structures
; -------------------------------------------------------------
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
(define (make-frame) (make-eq-hashtable))

(define (update-frame! frame var val)
  (hashtable-set! frame var val)
)

(define (bound-in-frame? frame var)
  (hashtable-contains? frame var)
)

(define (int->unnamed-var int)
  (cond
    ((eq? int 0) '$0) ((eq? int 1) '$1) ((eq? int 2) '$2)
    ((eq? int 3) '$3) ((eq? int 4) '$4) ((eq? int 5) '$5)
    ((eq? int 6) '$6) ((eq? int 7) '$7) ((eq? int 8) '$8)
    ((eq? int 9) '$9)
    (else (error "int->unnamed-var" "Invalid number" int))
  )
)

; Requirement: `frame` contains `var`
(define (frame-lookup frame var)
  (hashtable-ref frame var (void))
)

; Environment: a list of frames
; binginning with the local frame to the outermost
; environments are immutable, but frames can change
(define (local-frame env) (car env))
(define (outer-frames env) (cdr env))
(define empty-env '())
(define (extend-env env frame) (cons frame env))

; Variable lookup through the entire environment
; Unnamed variables should rarely be inherited from the enclosing...
; ...environment, unless it's intentionally done (Like in Currying).
(define (env-lookup var env)
  (cond
    ((eq? env empty-env) (error "env-lookup" "Unbound variable" var))
    (
      (bound-in-frame? (local-frame env) var)
      (frame-lookup (local-frame env) var)
    )
    (else (env-lookup var (outer-frames env)))
  )
)

; Assignment: can only change the local frame
; The value must be immediately evaluated in the current environment
; Note that the variable is not evaluated, this is one of the rare cases...
; ...where we do not evaluate something before acting on it
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
  (let [(probably-last-val (eval (car sequence) env))]
    (if (not (null? (cdr sequence)))
      (eval-seq (cdr sequence) env)  ; Not last
      probably-last-val  ; Last
    )
  )
)

; Much like map for eval
(define (eval-many exps env)
  (if (null? exps)
      '()
      (cons (eval (car exps) env)
            (eval-many (cdr exps) env)
      )
  )
)

; Function application
; The code body is evaluated in the enclosing environment...
; ...while the execution is done in a new local environment
; It's not strange that `eval` is called twice, since each call...
; ...bears a different meaning
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

; Make sure you don't provide unnamed vars as both named and unnamed
(define (frame-init->frame frame-init env)
  (define (build-frame frame-init frame unnamed-count)
    (if (null? frame-init)
      frame
      (begin
        (let ((first (car frame-init)) (rest (cdr frame-init)))
          (if (tagged? first '**)
            ; Named variable of the form (** <val> <val>)
            (update-frame! frame
              (cadr first)
              (eval (caddr first) env)
            )
            ; Unnamed variable
            (update-frame! frame
              (int->unnamed-var unnamed-count)
              (eval first env)
            )
          )
          (build-frame rest frame (+ 1 unnamed-count))
        )
      )
    )
  )

  (build-frame frame-init (make-frame) 0)
)

(define (fork-env frame-init base-env)
  (extend-env base-env (frame-init->frame frame-init base-env))
)

; Built-in stuff
(define prim-proc-table
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

(define (prim-proc? proc) (hashtable-contains? prim-proc-table proc))

; Vanilla, primitive Scheme application
(define (apply-prim-proc proc args)
  (apply (hashtable-ref prim-proc-table proc (void))
         args
  )
)

; The initial frame: contain compound masks for primitive procedures
(define (make-the-frame)
  (let ((the-frame (make-frame)))
    (update-frame! the-frame 'add  '(+ $0 $1))
    (update-frame! the-frame 'mult '(* $0 $1))
    (update-frame! the-frame 'subtract '(- $0 $1))

    the-frame
  )
)

; The global environment with an (almost) empty frame
(define global-env (list (make-the-frame)))


; ------------------------------------------------------------
; The Repl
; ------------------------------------------------------------
(define input-prompt "K>>> ")

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




