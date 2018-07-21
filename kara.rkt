#lang racket


(provide keval)
(provide interpret)
(provide repl)


; Evaluation = Analysis[Environment]
(define (keval exp env) ((analyze exp) env))

; The main switch block: the syntax analyzer.
; It returns a function which takes an environment
; and return the final evaluation.
(define (analyze exp)
    (cond
        [(self-eval? exp) (lambda (env) exp)]
        [(quoted? exp) (analyze-quoted (quoted-text exp) 1)]
        [(primitive? exp) (analyze-primitive exp)]
        [(unquoted? exp)
            (error "analyze" "Unquote used in wrong context" exp)]

        [(prim-proc-name? exp) (analyze-prim-proc-name exp)]
        [(var? exp) (lambda (env) (env-lookup exp env))]
        [(def? exp) (analyze-def exp)]
        [(if? exp) (analyze-if exp)]
        [(when? exp) (analyze-if (when->if exp))]
        [(cond? exp) (analyze-if (cond->if exp))]
        [(seq? exp) (analyze-seq exp)]

        [(lambda? exp) (analyze-lambda exp)]
        [(const? exp) (analyze-lambda (const->lambda exp))]
        [(delay? exp) (analyze-delay exp)]
        [(force? exp) (analyze-force exp)]
        ; Special commands
        [(env-request? exp) (lambda (env) env)]
        [(trace-command? exp) (analyze-trace-command exp)]
        ; Code execution is last
        [(pair? exp) (analyze-application exp)]
        [else (error "analyze" "Invalid expression" exp)]))


; Represent null record, used in hashtables.
(define null-record "null-record")

; I don't know why this is missing from Racket
(define (atom? exp) (not (or (null? exp)
                             (pair? exp))))

(define traced-functions (make-hash))




; -------------------------------------------------------------
; Arbitrary Constants
; If you hate my choices of notation, go ahead and change them
; but remember to change them in the code, too.
; -------------------------------------------------------------
(define KEYWORD_TAG     '~)
(define UNNAMED_PREFIX  '$)
(define SEQUENCE_TAG    'seq)
(define ASGN_TAG        'set!)
(define COND_TAG        'cond)
(define ENV_REQUEST_TAG 'meta-env)
(define PRIMITIVE_TAG   '!)



; -------------------------------------------------------------
; Types of expressions and their structures
; -------------------------------------------------------------
(define (tagged? exp tag) (and (pair? exp)
                               (eq? (car exp) tag)))

(define (self-eval? exp)
    (or (number? exp)
        (string? exp)
        (eq? exp #t)
        (eq? exp #f)))

(define (var? exp) (symbol? exp))

(define (asgn? exp) (tagged? exp ASGN_TAG))
(define (asgn-var exp) (cadr exp))
(define (asgn-val exp) (caddr exp))

(define (quoted? exp) (tagged? exp 'quote))
(define (quoted-text exp) (cadr exp))

(define (unquoted? exp) (tagged? exp 'unquote))
(define (unquoted-text exp) (cadr exp))
(define (unquoted-data exp)
    (if (unquoted? exp)
        (unquoted-data (unquoted-text exp))
        exp))
(define (unquote-level exp)
    (if (unquoted? exp)
        (+ 1 (unquote-level (unquoted-text exp)))
        0))

(define (trace-command? exp)
    (tagged? exp 'trace))

(define (env-request? exp) (tagged? exp ENV_REQUEST_TAG))

(define (primitive? exp) (tagged? exp PRIMITIVE_TAG))
(define (primitive-body exp) (cadr exp))

(define (if? exp) (tagged? exp 'if))
(define (make-if pred conse alt)
    (list 'if pred conse alt))
(define (if-pred exp) (cadr exp))
(define (if-conse exp) (caddr exp))
(define (if-alt exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        #f))

(define (cond? exp) (tagged? exp COND_TAG))
(define (cond-clauses cond-exp) (cdr cond-exp))
(define (cond-else-clause? clause) (eq? (car clause) 'else))
(define (cond-pred clause) (car clause))
; Note: you actually get a secret `seq` in `cond`,
; juts like in Scheme.
(define (cond-actions clause) (cdr clause))

(define (when? exp) (tagged? exp 'when))
(define (when-cond exp) (cadr exp))
(define (when-actions exp) (cddr exp))

(define (def? exp)
    (tagged? exp 'def))

(define (definition-of-data? exp)
    (symbol? (cadr exp)))

(define (def-var exp)
    (if (definition-of-data? exp)
        ; Data
        (cadr exp)
        ; Code
        (caadr exp)))

; Get the value of the definition regardless of whether
; it's data or code.
(define (def-val exp)
    (if (definition-of-data? exp)
        ; Data
        (caddr exp)
        ; Code: turn that to a lambda, then it will finally
        ; be evaluated to a procedure.
        (make-lambda (cdadr exp)    ; formal parameters (cdr (car (cdr...)))
                     (cddr exp))))  ; body

(define (lambda? exp) (tagged? exp 'lambda))
(define (lambda-params exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
; Constructor for lambda, used by `def-val
(define (make-lambda params body)
    (cons 'lambda (cons params body)))

(define (const? exp)
    (tagged? exp 'const))
(define (const-body const)
    (cdr const))

; A name is NOT the procedure.
(define (prim-proc-name? proc)
    (hash-has-key? prim-procs proc))
; A primitive procedure is a tagged Scheme function.
(define (prim-proc? exp)
    (tagged? exp 'primitive-implementation))
(define (apply-prim-proc proc args)
    (apply (cadr proc) args))
; Requirement: the procedure is primitive (in the table)
(define (get-primitive-implementation prim-proc-name)
    (hash-ref prim-procs prim-proc-name null-record))
(define (compound-proc? proc)
    (tagged? proc 'proc))

(define (seq? exp) (tagged? exp SEQUENCE_TAG))
(define (seq-actions seq) (cdr seq))
(define (make-seq actions) (cons 'seq actions))

(define (operator application) (car application))
(define (operands application) (cdr application))

(define (delay? exp) (tagged? exp 'delay))
(define (force? exp) (tagged? exp 'force))

(define (delayed-code exp) (cdr exp))
(define (forced-promise exp) (cdr exp))
(define (thunk? exp) (tagged? exp' thunk))
; Thunk is composed of analyzed code and the lexical environment.
(define (make-thunk code env) (list 'thunk code env))
(define (thunk-code exp) (cadr exp))
(define (thunk-env exp) (caddr exp))




; -----------------------------------------------------------
; Analysis
; -----------------------------------------------------------
; The Environment
; Bindings: Variable-Value pairs (or something similar)
(define (binding-var binding) (car binding))
(define (binding-val binding) (cadr binding))

; Frame: a hash table to store bindings
(define (new-frame) (make-hash))

(define (update-frame! frame var val)
    (hash-set! frame var val))

; Lookup on failure returns a null record.
(define (frame-lookup frame var)
    (hash-ref frame var null-record))

; Environment: a list of frames starting with the local
; frame and ending with the outermost frame.
; Environments are immutable, but frames can change (keep that in mind).
(define (local-frame env) (car env))
(define (outer-frames env) (cdr env))
(define empty-env '())

; Extending is the act of prepending an environment...
; with a new local frame
(define (extend-env env frame) (cons frame env))

; Variable lookup through the entire environment.
(define (env-lookup var env)
    (if (eq? env empty-env)
        (error "env-lookup" "Var not bound in env" var)
        (let ([lookup (frame-lookup (local-frame env) var)])
            (if (eq? lookup null-record)
                (env-lookup var (outer-frames env))
                lookup))))

; Assignment statements can only affect the local frame.
; The value is immediately evaluated in the current environment.
; Note that the defined variable is not evaluated.
(define (analyze-def definition)
    (define var (def-var definition))
    (define vproc (analyze (def-val definition)))
    (lambda (env)
        (update-frame! (local-frame env)
                       var
                       (vproc env))
        'ok))

; The conditionals
(define (analyze-if exp)
    (let((pproc (analyze (if-pred exp)))
         (cproc (analyze (if-conse exp)))
         (aproc (analyze (if-alt exp))))
        (lambda (env)
            (if (pproc env)
                (cproc env)
                (aproc env)))))

; The heart of `cond->if`
; Note: you actually get an implicit `seq`, juts like in Scheme.
(define (expand-clauses clauses)
    (if (null? clauses)
        ; If null there is no else clause
        #f
        (let ((cur-clause (car clauses)))
            (if (cond-else-clause? cur-clause)
                ; Warning: `else` clause should be the last clause
                (make-seq (cond-actions cur-clause))
                (make-if (cond-pred cur-clause)
                         (make-seq (cond-actions cur-clause))
                         (expand-clauses (cdr clauses)))))))

(define (cond->if exp)
    (expand-clauses (cond-clauses exp)))

(define (when->if when-exp)
    (make-if (when-cond when-exp) (make-seq (when-actions when-exp)) #f))

; Analyze a sequence
(define (analyze-seq seq-exp)
    ; Function executing two analyzed expressions...
    ; returning the latter result.
    (define (chain proc1 proc2)
        (lambda (env) (proc1 env) (proc2 env)))

    (define (core things-to-do things-left)
        (if (null? things-left)
            things-to-do
            (core
                (chain things-to-do (car things-left))
                (cdr things-left))))

    (define analyzed-actions (map analyze (seq-actions seq-exp)))

    (if (null? analyzed-actions)
        (raise "Empty sequence")
        (core (car analyzed-actions) (cdr analyzed-actions))))

(define (const->lambda exp)
    (make-lambda '() (const-body exp)))

; The function name is not evaluated.
(define (analyze-trace-command exp)
    (hash-set! traced-functions (cadr exp) #t)
    ; But you have to return something here
    (lambda (env) (format "Traced ~s" (cadr exp))))


; A procedure is defined as follow:
(define (make-proc parameters body env)
    (list 'proc parameters body env))

(define (proc? exp)
    (tagged? exp 'proc))

(define (proc-params proc) (cadr proc))
(define (proc-body proc) (caddr proc))
; The procedure's lexical scope when it is evaluated.
(define (proc-env proc) (cadddr proc))

; Lambda is the interface to making a new procedure.
(define (analyze-lambda exp)
    (define params (lambda-params exp))
    ; Reuse analyze-seq because we can
    (define bproc (analyze-seq (make-seq (lambda-body exp))))
    (lambda (env) (make-proc params bproc env)))

(define (analyze-prim-proc-name prim-proc-name)
    (define impl (get-primitive-implementation prim-proc-name))
    (lambda (env) (list 'primitive-implementation impl)))
    
; Now this one is complicated and very different from quasiquotation
(define (analyze-quoted exp q-level)
    (cond
        [(quoted? exp)
            (let ([analyzed-text (analyze-quoted (quoted-text exp)
                                                 (+ q-level 1))])
                 (lambda (env) `(quote ,(analyzed-text env))))]

        [(= (unquote-level exp) q-level) (analyze (unquoted-data exp))]
        [(> (unquote-level exp) q-level)
            (error "analyze-quoted" "Too many unquotes!" exp)]
        [(pair? exp)
            (let ([analyzed-items
                   (map (lambda (e) (analyze-quoted e q-level)) exp)])
                 (lambda (env) (map (lambda (a) (a env)) analyzed-items)))]

        [(atom? exp) (lambda (env) exp)]
        [(null? exp) (lambda (env) (list))]
        [else (error "analyze-quoted" "What 'else' did we miss?" exp)]))

; Primitives are not evaluated before fed in to Scheme.
(define (analyze-primitive exp)
    (lambda (env) (eval (primitive-body exp))))

(define (analyze-delay exp)
    (define delayed-analyzed (analyze (delayed-code exp)))
    (lambda (env)
        (make-thunk delayed-analyzed env)))

(define (analyze-force exp)
    (lambda (env)
        (define maybe-thunk ((forced-promise exp) env))
        (if (thunk? maybe-thunk)
            ((thunk-code maybe-thunk) env)
            (error "analyze-force" "Expression not a thunk" maybe-thunk))))


; -----------------------------------------------------------
; Code Execution
; -----------------------------------------------------------

; Analyze a function execution.
; The operator expression is fetched from the caller's environment
; while the execution is done by evaluating in a forked environment.
(define (analyze-application exp)
    (define $operator (operator exp))
    (define $operands (operands exp))
    (define fproc (analyze $operator))
    (define aprocs (map analyze $operands))

    (lambda (env)
        ; When a traced function is called, notify the user
        (when (traced? $operator)
            (begin (display $operator) (newline)))
        ; The main job is done here
        (execute-application (fproc env)
                             (map (lambda (aproc) (aproc env))
                                  aprocs))))

; Called from `analyze-application`
(define (execute-application proc args)
    (cond [(prim-proc? proc) (apply-prim-proc proc args)]
          [(compound-proc? proc)
           (define params (proc-params proc))
           ; `params` can just be an atom
           ; (a single rest argument is passed in).
           ; This design is very clever, since it covers
           ; both `define` and `lambda`.
           (when (and (list? params)
                      (not (eq? (length params)
                                (length args))))
                 (error "analyze-application"
                        "Arity mismatch"
                        (list proc args)))
           ; The new frame initialized by the arguments
           (define exec-frame
               (zip-and-make-frame (new-frame) params args))
           ; The environment to run the code
           (define new-env (extend-env (proc-env proc) exec-frame))
           ; Finally, run the code (already analyzed) in
           ; the new environment
           (define body (proc-body proc))
           (body new-env)]
          [else (error "analyze-application"
                       "Non-procedure application"
                       proc)]))

; Used by `analyze-application`
(define (zip-and-make-frame frame first second)
    (cond [(null? first) frame]
          ; Normal argument incoming if it is a pair
          ; (including improper list in case of rest argument)
          [(pair? first)
           (begin (update-frame! frame (car first) (car second))
                  (zip-and-make-frame frame (cdr first) (cdr second)))]
          ; Rest arg: this must be the final argument,
          ; so we must also return
          [else (begin (update-frame! frame first second)
                       frame)]))

(define (traced? func)
    (hash-has-key? traced-functions func))

; -----------------------------------------------------
; Built-in Procedures and Initialization
; -----------------------------------------------------

(define output-prompt "K>>> ")

(define (display-output output)
    (display output-prompt) (display output) (newline))

; The interpreter
(define (interpret exp) (keval exp global-env))
; The file parser
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

(define prim-procs (make-hash))
; List
(hash-set! prim-procs 'car car)
(hash-set! prim-procs 'cdr cdr)
(hash-set! prim-procs 'cons cons)
(hash-set! prim-procs 'append append)
(hash-set! prim-procs 'list list)
(hash-set! prim-procs 'null? null?)
(hash-set! prim-procs 'list-ref list-ref)
; Arithmetic
(hash-set! prim-procs '+ +)
(hash-set! prim-procs '- -)
(hash-set! prim-procs '* *)
(hash-set! prim-procs '/ /)
(hash-set! prim-procs '= =)
(hash-set! prim-procs 'remainder remainder)
(hash-set! prim-procs 'even? even?)
(hash-set! prim-procs 'not not)
(hash-set! prim-procs 'random random)
; Hashtable
(hash-set! prim-procs 'make-hash make-hash)
(hash-set! prim-procs 'hash-has-key? hash-has-key?)
(hash-set! prim-procs 'hash-ref? hash-ref)
(hash-set! prim-procs 'hash-set! hash-set!)
(hash-set! prim-procs 'hash-keys hash-keys)
; System
(hash-set! prim-procs 'raise raise)
(hash-set! prim-procs 'error error)
; I/O
(hash-set! prim-procs 'current-output-port current-output-port)
(hash-set! prim-procs 'format format)
(hash-set! prim-procs 'newline newline)
(hash-set! prim-procs 'display display)
(hash-set! prim-procs 'load kload)
; Others
(hash-set! prim-procs 'random random)
(hash-set! prim-procs 'void void)
(define The-frame (new-frame))

; The global environment with The frame
(define global-env (list The-frame))

; The common library writen in Kara
(display "Loading the Common Library...") (newline)
(kload "common.kar")



; ------------------------------------------------------------
; The Repl
; ------------------------------------------------------------

(define input-prompt "K<<< ")


(define (repl)
    (display input-prompt)
    (let ((input (read)))
        (display-output (interpret input)))
    (repl))






