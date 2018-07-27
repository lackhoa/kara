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
        [(pmatch? exp) (analyze-pmatch exp)]
        ; Special commands
        [(trace-command? exp) (analyze-trace-command exp)]
        [(untrace-command? exp) (analyze-untrace-command exp)]
        ; Code execution is last (must be a property list)
        [(list? exp) (analyze-application exp)]
        [else (error "analyze" "Invalid expression" exp)]))


; Represent null record, used in hashtables.
(define null-record "null-record")

; I don't know why this is missing from Racket
(define (atom? exp) (not (or (null? exp)
                             (pair? exp))))

(define traced-procs (make-hash))




; -----------------------------------------------------------
; Arbitrary Constants
; If you hate my choices of notation, go ahead and change them
; but remember to change them in the code, too.
; -----------------------------------------------------------
(define KEYWORD_TAG     '~)
(define UNNAMED_PREFIX  '$)
(define SEQUENCE_TAG    'begin)
(define ASGN_TAG        'set!)
(define COND_TAG        'cond)



; -----------------------------------------------------------
; Types of expressions and their structures
; -----------------------------------------------------------
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

(define (untrace-command? exp)
    (tagged? exp 'untrace))

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
; Note: you actually get a secret `begin` in `cond`,
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
; it's data or code. Note that we also do the analyzing work here.
(define (def-val exp)
    (if (definition-of-data? exp)
        ; Data
        (analyze (list-ref exp 2))
        ; Code: turn that into a procedure
        (let ([bproc (analyze-seq (make-seq (list-tail exp 2)))]
              [name (car (list-ref exp 1))])
            (when (prim-proc-name? name)
                  (error "def-val" "You are shadowing a primitive: ~s" name))
            (lambda (env)
                (make-proc name
                           (cdr (list-ref exp 1))  ; formal parameters
                           bproc                   ; body
                           env)))))                ; lexical environment

(define (lambda? exp) (tagged? exp 'lam))

; Used to convert const to lambda
(define (make-lambda params body)
    (cons 'lambda (cons params body)))

(define (lambda-params exp) (list-ref exp 1))
(define (lambda-body exp) (list-tail exp 2))

(define (const? exp)
    (tagged? exp 'const))
(define (const-body const)
    (cdr const))

; -----------------------------------------------------------
; Primitive Procedure
; -----------------------------------------------------------
; Note that primitive procedures don't have parameters or environments
; since they were created outside of the language.

; Be careful to distinguish the procedure from its name,
; which needs not be the same as that of the hosting Scheme.
(define (prim-proc-name? exp)
    (hash-has-key? prim-procs exp))

; Requirement: the procedure is indeed primitive
(define (get-implementation prim-proc-name)
    (hash-ref prim-procs prim-proc-name null-record))

(define (prim-proc? exp)
    (tagged? exp 'prim-proc))

(define (make-prim-proc name body)
    (list 'prim-proc name body))

(define (prim-proc-body prim-proc)
    (list-ref prim-proc 2))

(define (apply-prim-proc prim-proc args)
    (apply (prim-proc-body prim-proc) args))

; -----------------------------------------------------------
; Compound Procedure (or just Procedure for short)
; -----------------------------------------------------------

(define (proc? exp)
    (tagged? exp 'proc))

; The body of a procedure is an analyzed chunk of code.
(define (make-proc name params body env)
    (list 'proc name params body env))

(define (proc-params proc) (list-ref proc 2))
(define (proc-body proc) (list-ref proc 3))
; The procedure's lexical scope when it is evaluated.
(define (proc-env proc) (list-ref proc 4))

; -----------------------------------------------------------
; Common procedure interface
; -----------------------------------------------------------
(define (proc-name proc)
    (list-ref proc 1))

(define (seq? exp) (tagged? exp SEQUENCE_TAG))
(define (seq-actions seq) (cdr seq))
(define (make-seq actions) (cons SEQUENCE_TAG actions))

(define (operator application) (car application))
(define (operands application) (cdr application))

(define (delay? exp) (tagged? exp 'delay))
(define (force? exp) (tagged? exp 'force))

(define (delayed-code exp) (cadr exp))
(define (force-promise exp) (cadr exp))
(define (thunk? exp) (tagged? exp' thunk))
; Thunk is composed of analyzed code and the lexical environment.
(define (make-thunk code env) (list 'thunk code env))
(define (thunk-code exp) (cadr exp))
(define (thunk-env exp) (caddr exp))

; Pattern matching
(define (pmatch? exp) (tagged? exp 'case))
(define (pmatch-val pmatch)     (cadr pmatch))
(define (pmatch-clauses pmatch) (list-tail pmatch 2))
(define (pmatch-clause-pat clause)     (car clause))
(define (pmatch-clause-actions clause) (cdr clause))
(define (pat-constructor pat)
    (if (list? pat)
        (car pat)
        (error "pat-constructor" "Illegal pattern" pat)))
(define (pat-vars pat)
    (if (list? pat)
        (cdr pat)
        (error "pat-constructor" "Illegal pattern" pat)))

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
    ; Note that the value has already been analyzed
    (define val (def-val definition))
    (lambda (env)
        (update-frame! (local-frame env)
                       var
                       (val env))
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
; Note: you actually get an implicit `begin`, juts like in Scheme.
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
    (hash-set! traced-procs (cadr exp) #t)
    ; But you have to return something here
    (lambda (env) (format "Traced ~s" (cadr exp))))

(define (analyze-untrace-command exp)
    (hash-remove! traced-procs (cadr exp))
    ; But you have to return something here
    (lambda (env) (format "Untraced ~s" (cadr exp))))


; Lambda is the interface to making a new procedure.
(define (analyze-lambda exp)
    (define params (lambda-params exp))
    (define body (lambda-body exp))
    ; Reuse analyze-seq because we can
    (define bproc (analyze-seq (make-seq body)))
    ; The name of the procedure is the entire lambda expression
    ; So you shouldn't make a long function and not name it
    (lambda (env) (make-proc exp params bproc env)))

(define (analyze-prim-proc-name prim-proc-name)
    (define impl (get-implementation prim-proc-name))
    (lambda (env) (make-prim-proc prim-proc-name impl)))
    
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

; Delay will create a thunk object, containing
; 1. the analyzed code and,
; 2. the environment to run that code in.
(define (analyze-delay exp)
    (define delayed-analyzed (analyze (delayed-code exp)))
    (lambda (env)
        (make-thunk delayed-analyzed env)))

(define (analyze-force exp)
    (define promise (analyze (force-promise exp)))
    (lambda (env)
        (define maybe-thunk (promise env))
        (if (thunk? maybe-thunk)
            ; Compute the thunk's code inside of its own environment
            ((thunk-code maybe-thunk) (thunk-env maybe-thunk))
            ; If already forced then don't bother
            maybe-thunk)))

(define (analyze-pmatch pmatch)
    (define aval     (analyze (pmatch-val pmatch)))
    (define processed-clauses (process-pmatch-clauses (pmatch-clauses pmatch)))
    (lambda (env)
        (let ([val (aval env)])
             ; If the value is not a list then no pattern matching for you
             (if (list? val)
                 (match-loop val processed-clauses env)
                 #f))))

; Used by `analyze-pmatch`
(define (match-loop val clauses env)
    (if (null? clauses)
        ; It's still debated whether or not partial pattern matching
        ; is a thing
        (error "analyze-match" "Matching failed" (my-output-format val))
        (let* ([clause (car clauses)]
               [pred (car clause)]
               [vars (cadr clause)]
               [analyzed-actions (list-ref clause 2)])
            (if (pred val)
                ; This is precisely the same pattern as function application
                (let* ([exec-frame (zip-and-make-frame (new-frame)
                                                       vars
                                                       (cdr val))]
                       [new-env (extend-env env exec-frame)])
                    ; Run the code
                    (analyzed-actions new-env))
                (match-loop val (cdr clauses) env)))))

; Used by analyze-pmatch
; Analyze the patterns' requirement the actions in the clauses
(define (process-pmatch-clauses clauses)
    (define (analyze-clause clause)
        (define pat (pmatch-clause-pat clause))
        (define pred (lambda (val)
                        (and (= (length pat) (length val))
                             (eq? (car val) (pat-constructor pat)))))
        (define analyzed-actions
            (analyze-seq (make-seq (pmatch-clause-actions clause))))
        ; After analyzing, we have the predicate,
        ; the variables' names, and the analyzed actions
        (list pred (pat-vars pat) analyzed-actions))

    (map analyze-clause clauses))

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
        (execute-application (fproc env)
                             (map (lambda (aproc) (aproc env))
                                  aprocs))))

; Called from `analyze-application`
(define (execute-application proc args)
    (define name (proc-name proc))
    (cond [(prim-proc? proc) (trace-notify name args)
                             (apply-prim-proc proc args)]

          [(proc? proc)
           (trace-notify name args)
           (define params (proc-params proc))
           ; Note: `params` can be an atom (a single 'rest' argument is passed in).
           ; This design is very clever, since it covers both `define` and `lambda`.
           (when (and (list? params)
                      (not (eq? (length params)
                                (length args))))
                 (error "analyze-application"
                        (format "Arity mismatch in ~s, expected ~s, got ~s"
                                name
                                (length params)
                                (length args))
                        (cons name args)))
           ; The new frame initialized by the arguments
           (define exec-frame
               (zip-and-make-frame (new-frame) params args))
           ; Finally, run the (already analyzed) code in the new environment.
           (define new-env (extend-env (proc-env proc) exec-frame))
           ((proc-body proc) new-env)]

          [else (error "analyze-application"
                       "Non-procedure application"
                       proc)]))

; Used by both `analyze-application` (originally)
; and pattern matching
(define (zip-and-make-frame frame first second)
    (cond [(null? first) frame]

          ; Normal argument incoming if it is a pair
          ; (including improper list in case of 'rest' argument)
          [(pair? first)
           (begin (update-frame! frame (car first) (car second))
                  (zip-and-make-frame frame (cdr first) (cdr second)))]

          ; Rest argument: this must be the final argument, so we return the frame
          [else (update-frame! frame first second)
                frame]))

(define (traced? name)
    (hash-has-key? traced-procs name))

; When a traced procedure is called, notify the user
(define (trace-notify name args)
    (when (traced? name)
        (display (format "(~s ~s)" name args))
        (newline)))

; -----------------------------------------------------------
; Built-in Procedures and Initialization
; -----------------------------------------------------------

(define output-prompt "K>>> ")

(define (my-output-format output)
    (cond
        [(proc? output)
         ; Don't print the environment and Scheme's procedure
         (format "[compound-procedure ~s ~s <Scheme-procedure> <proc-environment>]"
                 (proc-name output)
                 (proc-params output))]
        [(thunk? output)
         ; Don't print the thunk's environment
         (format "[thunk <thunk-code> <thunk-environment>]")]
        [(pair? output)
         ; This part is a slight hack, our list representation for
         ; special values is flawed.
         (cons (my-output-format (car output))
               (my-output-format (cdr output)))]
        [else output]))

(define (display-output output)
    (display output-prompt)
    (display (my-output-format output))
    (newline))

; The interpreter
(define (interpret exp) (keval exp global-env))
; The file parser
(define (kload input-file-name)
    (let ([input (open-input-file input-file-name)])
        ; This guy uses named let
        (let reader ([next (read input)])
            (if (eof-object? next)
                (close-input-port input)
                (begin
                    (let ((interpreted (interpret next)))
                        (when
                            (not (or (eq? interpreted (void))
                                     (eq? interpreted 'ok)))
                            (display-output interpreted)))
                    (reader (read input)))))))

(define prim-procs (make-hash))
; Operations on Objects
(hash-set! prim-procs 'eq? eq?)
(hash-set! prim-procs 'eqv? eqv?)
(hash-set! prim-procs 'equal? equal?)
(hash-set! prim-procs 'atom? atom?)
(hash-set! prim-procs 'null? null?)
(hash-set! prim-procs 'pair? pair?)
(hash-set! prim-procs 'list? list?)
(hash-set! prim-procs 'number? number?)
; Strict List (prepended with "l")
(hash-set! prim-procs 'list list)
(hash-set! prim-procs 'cons cons)
(hash-set! prim-procs 'lcar car)
(hash-set! prim-procs 'lcadr cadr)
(hash-set! prim-procs 'lcdr cdr)
(hash-set! prim-procs 'lappend append)
(hash-set! prim-procs 'list-ref list-ref)
; Arithmetic
(hash-set! prim-procs '+ +)
(hash-set! prim-procs '- -)
(hash-set! prim-procs '* *)
(hash-set! prim-procs '/ /)
(hash-set! prim-procs '= =)
(hash-set! prim-procs '< <)
(hash-set! prim-procs 'remainder remainder)
(hash-set! prim-procs 'even? even?)
(hash-set! prim-procs 'not not)
(hash-set! prim-procs 'random random)
(hash-set! prim-procs 'and (lambda (p q) (and p q)))
(hash-set! prim-procs 'or (lambda (p q) (or p q)))
(hash-set! prim-procs 'nand (lambda (p q) (nand p q)))
(hash-set! prim-procs 'nor (lambda (p q) (nor p q)))
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
; Time
(hash-set! prim-procs 'time-s current-seconds)
(hash-set! prim-procs 'time-ms current-inexact-milliseconds)
; Others
(hash-set! prim-procs 'random random)
(hash-set! prim-procs 'void void)

; The global environment with The frame
(define The-frame (new-frame))
(define global-env (list The-frame))

; The common library writen in Kara
(kload "common.kar")



; -----------------------------------------------------------
; The Repl
; -----------------------------------------------------------

(define input-prompt "K<<< ")


(define (repl)
    (display input-prompt)
    (define input (read))
    (when (nor (eq? input eof)
               (equal? input '(exit)))
        (display-output (interpret input))
        (repl)))






