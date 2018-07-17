#lang racket

(provide keval)
(provide new-frame)
(provide update-frame!)

; Represent null record, used in hashtables.
(define null-record "null-record")

; I don't know why this is missing from Racket
(define (atom? exp) (not (or (null? exp)
                             (pair? exp))))

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

        [(env-request? exp) (lambda (env) env)]
        [(var? exp) (lambda (env) (env-lookup exp env))]
        [(asgn? exp) (analyze-asgn exp)]
        [(if? exp) (analyze-if exp)]
        [(cond? exp) (analyze-if (cond->if exp))]
        [(seq? exp) (analyze-seq exp)]
        ; Code execution is last
        [(pair? exp) (analyze-exec exp)]
        [else (error "analyze" "Invalid expression" exp)]))

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
        [(null? exp) (lambda (env) '())]
        [else (error "analyze-quoted" "What 'else' did we miss?" exp)]))

(define (analyze-primitive exp)
    (let ([analyzed-body (analyze (primitive-body exp))])
        (lambda (env) (eval (analyzed-body env)))))
        
; This hashtable remembers the analyzation of functions, because...
; we functions can be analyzed many times, and we can't distinguish them...
; from expressions
(define analysis-table null)
(set! analysis-table (make-hash))


; -------------------------------------------------------------
; Arbitrary Constants
; If you hate my choices of notation, go ahead and change them
; but remember to change them in the code, too.
; -------------------------------------------------------------
(define KEYWORD_TAG '~)
(define UNNAMED_PREFIX '$)
(define SEQUENCE_TAG 'seq)
(define ASGN_TAG 'set!)
(define COND_TAG 'cond)
(define ENV_REQUEST_TAG 'meta-env)
(define PRIMITIVE_TAG '!)

; -------------------------------------------------------------
; Types of expressions and their structures
; -------------------------------------------------------------
(define (tagged? exp tag) (and (pair? exp)
                               (eq? (car exp) tag)))

(define (self-eval? exp)
    (or (number? exp) (string? exp) (eq? exp #t) (eq? exp #f)))

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

(define (env-request? exp) (eq? exp ENV_REQUEST_TAG))

(define (primitive? exp) (tagged? exp PRIMITIVE_TAG))
(define (primitive-body exp) (cadr exp))

(define (if? exp) (tagged? exp 'if))
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
(define (cond-actions clause) (cadr clause))

(define (seq? exp) (tagged? exp SEQUENCE_TAG))
(define (seq-actions seq) (cdr seq))

; Compound procedure _call_, not _application_.
(define (operator exp) (car exp))
(define (call-binding-exps exp) (cdr exp))


; -----------------------------------------------------------
; Analysis and execution
; -----------------------------------------------------------
; The Environment
; Bindings: Variable-Value pairs (or something similar)
(define (binding-var binding) (car binding))
(define (binding-val binding) (cadr binding))

; Frame: a hash table to store bindings
(define (new-frame) (make-hash))

(define (update-frame! frame var val)
    (hash-set! frame var val))

(define (int->unnamed-para int)
    (cond
        ((eq? int 0) '$0) ((eq? int 1) '$1) ((eq? int 2) '$2)
        ((eq? int 3) '$3) ((eq? int 4) '$4) ((eq? int 5) '$5)
        ((eq? int 6) '$6) ((eq? int 7) '$7) ((eq? int 8) '$8)
        ((eq? int 9) '$9)
        (else (error "int->unnamed-para" "Invalid number" int))))

(define (frame-lookup frame var)
    (hash-ref frame var null-record))

; Environment: a list of frames starting with the local...
; frame and ending with the outermost frame.
; Environments are immutable, but frames can change
(define (local-frame env) (car env))
(define (outer-frames env) (cdr env))
(define empty-env '())

; Extending is the act of prepending an environment...
; with a new local frame
(define (extend-env env frame) (cons frame env))

; Variable lookup through the entire environment.
; Unnamed parameters should NOT be inherited from the enclosing...
; environment, for a few intentional cases (e.g. Currying).
(define (env-lookup var env)
    (if (eq? env empty-env)
        (error "env-lookup" "Var not bound in env" var)
        (let ((lookup (frame-lookup (local-frame env) var)))
            (if (eq? lookup null-record)
                (env-lookup var (outer-frames env))
                lookup))))

; Assignment statements can only affect the local frame.
; The value is immediately evaluated in the current environment.
; Note that the variable on the left is not evaluated, this is one...
; of the rare cases where we do not evaluate something before acting on it
(define (analyze-asgn assignment)
    ; Using `let` since we want to analyze before...
    ; returning the lambda.
    (let ((vproc (analyze (asgn-val assignment))))
        (lambda (env)
            (update-frame! (local-frame env)
                (asgn-var assignment)
                (vproc env))
            'ok)))

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
(define (expand-clauses clauses)
    (if (null? clauses)
        #f
        (let ((cur-clause (car clauses)))
            (if (cond-else-clause? cur-clause)
                ; Warning: `else` clause should be the last clause
                (cond-actions cur-clause)
                (list
                    'if
                    (cond-pred cur-clause)
                    (cond-actions cur-clause)
                    (expand-clauses (cdr clauses)))))))

(define (cond->if exp)
    (expand-clauses (cond-clauses exp)))

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

    (let ((analyzed-actions (map analyze (seq-actions seq-exp))))
        (if (null? analyzed-actions)
            (raise "Empty sequence")
            (core (car analyzed-actions) (cdr analyzed-actions)))))

; Analyze a function execution
; The code expression is evaluated in the enclosing environment...
; while the execution is done by evaluating in a forked environment.
; It's not strange that `analyze` is called twice, since each call...
; bears a distinct meaning.
(define (analyze-exec exp)
    (let ((analyzed-operator (analyze (operator exp)))
          (bindings (binding-exps->bindings (call-binding-exps exp))))
        (lambda (env)
            (let*((eval-analyzed (analyzed-operator env))
                  (forked-env (extend-env env (bindings->frame bindings env)))
                  (lookup (hash-ref analysis-table eval-analyzed null-record)))
                (if (not (eq? lookup null-record))
                    (lookup forked-env)
                    (let ((doubly-analyzed (analyze eval-analyzed)))
                        (begin (hash-set! analysis-table eval-analyzed doubly-analyzed)
                               ; This is basically `keval`
                               (doubly-analyzed forked-env))))))))

; -----------------------------------------------------------
; Code Execution
; -----------------------------------------------------------
; Turn binding expressions to bindings, done in analysis.
; Note that the bindings' values must be applied to an environment.
; The variables in keyword expressions are NOT evaluated.
; Note: Make sure you don't provide unnamed parameters as named ones
; unless you know what you're doing.
(define (binding-exps->bindings binding-exp)
    (binding-exps->bindings-core binding-exp 0))

(define (binding-exps->bindings-core binding-exps unnamed-count)
    (if (null? binding-exps)
        ; No more binding left
        (quote ())
        ; More bindings to add
        (let ((first (car binding-exps)) (rest (cdr binding-exps)))
            (if (tagged? first KEYWORD_TAG)
                ; Named argument of the form "<KEYWORD_TAG> <var> <val>"
                (let ((analyzed-val (analyze (caddr first))))
                    (cons
                        (list (cadr first) analyzed-val)
                        (binding-exps->bindings-core rest unnamed-count)))
                ; Unnamed argument
                (let((analyzed-val (analyze first))
                     (unnamed-para (int->unnamed-para unnamed-count)))
                    (cons
                        (list unnamed-para analyzed-val)
                        (binding-exps->bindings-core rest (+ 1 unnamed-count))))))))

; Used solely by `fork-env`
(define (bindings->frame bindings env)
    (define (loop bindings frame)
        (if (null? bindings)
            frame
            (let ((first (car bindings)))
                ; The value provided must be applied to `env`
                (hash-set! frame (car first) ((cadr first) env))
                (loop (cdr bindings) frame))))
    (loop bindings (new-frame)))
