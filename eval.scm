(define
  (eval exp env)
  (cond
    ((self-eval? exp) exp)
    ((var? exp) (lookup-var exp env))
    ((quoted? exp) (quoted-text exp))
    ((asgn? exp) (eval-asgn exp env))
    ((if? exp) (eval-if exp env))
    (
      (lambda? exp)
      (make-proc (lambda-body exp) env)  ;No lambda parameters
    )
    ((begin? exp) (eval-seq (begin-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    (
      (application? exp)
      (apply
        (eval (operator exp) env)
        (map (lambda (x) (eval x env)) (operands exp))
      )
    )
    (else (error "Unknown expression type -- EVAL" exp))
  )
)

(define
  (apply proc args)
  (cond
    ((prim-proc? proc) (apply-prim-proc proc args))
    (
      (comp-proc? proc) 
      (eval-seq
        (proc-body proc)
        (extend-env args (proc-env proc))
      )
    )
    (else (error "Unknown procedure type -- APPLY" proc))
  )
)

(define
  (eval-if exp env)
  (if (true? (eval (if-pred exp) env))
      (eval (if-conse exp) env)
      (eval (if-alt exp) env)
  )
)

(define
  (eval-seq exps env)
  (cond ((last-exp? exps) (eval (car exps) env))
        (else
          (eval (car exps) env)
          (eval-seq (cdr exps) env)
        )
  )
)

(define
  (eval-asgn exp env)
  (set-var!
    (asgn-var exp)
    (eval (asgn-val exp) env)
    env
  )
  'ok
)

(define
  (eval-def exp env)
  (
    define-var!
    (defined-var exp)
    (eval (defined-val exp) env)
    env
  )
)

(define
  (self-eval? exp)
  (and (number? exp) (string? exp))
)

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged? exp 'quote))

(define (quoted-text exp) (cadr exp))

(define (tagged? exp tag) (and?
                            (pair? exp)
                            (eq? (car exp) tag)
                          )
)

(define (asgn? exp) (tagged? exp 'set!))

(define (asgn-var exp) (cadr exp))

(define (asgn-val exp) (caddr exp))

;Lambda: no parameters
(define (lambda? exp) (tagged exp 'lambda))

(define (lambda-body exp) (cdr exp))

(define (make-lambda body) (cons 'lambda body))

(define (if? exp) (tagged exp 'if))

(define (if-pred exp) (cadr exp))

(define (if-conse exp) (caddr exp))

(define (if-alt exp)
        (if (not (null? (cdddr exp)))
            (cadddr exp)
            #f  ;This part is made-up
        )
)

(define (make-if pred conse alt)
        (list 'if pred conse alt)
)

(define (begin? exp) (tagged exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (seq->exp seq)
        (cond ((null? seq) seq)
              ((last-exp? seq) (car seq))
              (else (make-begin seq))
        )
)

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operand exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (cond? exp) (tagged? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
        (eq? (cond-pred clause) 'else)
)

(define (cond-pred clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define
  (define
    (expand-clauses clauses)
    (if (cond-else-clause (car clauses))
        (if (null? (cdr clauses))
            (seq->exp (cond-actions (car clauses)))
            (error "ELSE clause isn't last -- COND->IF" clauses)
        )
        (make-if (cond-pred (car clauses))
                 (seq->exp (cond-actions (car clauses)))
                 (expand-clauses (cdr clauses))
        )
    )
  )
  (cond->if exp)
  (expand-clauses (cond-clauses exp))
)

(define (true? x)
        (and
          (not (eq? x #f))
          (not (eq? x '()))
        )
)

(define (false? x) (not (true? x)))

(define (make-proc body env)
        (list 'proc body env))

(define (comp-proc? p) (tagged? p 'proc))

(define (proc-body p) (caddr p))

(define (enclosing-env env) (cdr env))

;A frame is a list of var-val pairs
(define (var-of frame) (car frame))
(define (val-of frame) (cadr frame))

(define (first-frame env) (car env))

(define empty-env '())

(define (add-binding-to-frame! var val frame)
        (set! frame (cons '(var val) frame))
)

(define (extend-env frame base-env)  ;Returns a new environment
        (cons frame base-env)
)

(define (lookup-var var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame) (env-loop (enclosing-env env)))
            ((eq? (var-of (car frame)) var) (val-of (car frame)))
            (else (scan (cdr frame)))
      )
    )
    (if (eq? env empty-env)
        (error "Unbound variable" var)
        (scan (first-frame env))
    )
  )
  (env-loop env)
)

;Only changes the first frame it finds the variable in
(define (set-var var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame) (env-loop (enclosing-env env)))
            ((eq? (var-of (car frame)) var) (set-car! frame '(var val)))
            (else (scan (cdr frame)))
      )
    )
    (if (eq? env empty-env)
        (error "Unbound variable" var)
        (scan (first-frame env))
    )
  )
  (env-loop env)
)


(define (define-var! var val env)
  (set! this-frame (first-frame env))
  (define (scan frame)
    (cond ((null? frame) (add-binding-to-frame! var val this-frame))
          ((eq? var (var-of (car frame))) (set-car! frame '(var val))
          (else (scan (cdr frame))
    )
  )
  (scan (first-frame env))
)

























