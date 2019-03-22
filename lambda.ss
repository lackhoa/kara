(load "lookup.ss")
(define CLOS  (lambda (env var body) `(CLOS ,env ,var ,body)))
(define N-var (lambda (name) `(N-var ,name)))
(define N-ap  (lambda (rator rand) `(N-ap ,rator ,rand)))

(define val
  (lambda (rho e)
    (pmatch e
      "val"
      [(lambda (,x) ,b) (CLOS rho x b)]
      [,x (guard (symbol? x))
          (cond
           [(assv x rho) => rhs]
           [else (error 'val "Unknown variable" x)])]
      [(,rator ,rand) (do-ap (val rho rator) (val rho rand))])))

(define do-ap
  (lambda (fun arg)
    (pmatch fun
      "do-ap"
      [(CLOS ,rho ,x ,b) (val (extend rho x arg) b)]
      [;; Will only be met if called from read-back
       else `(N-ap ,fun ,arg)])))

(define run-program
  (lambda (rho exprs)
    (pmatch exprs
      "run-program"
      [() (void)]
      [((define ,x ,e) . ,rest)
       (let ([v (val rho e)])
         (run-program (extend rho x v) rest))]
      [(,e . ,rest)
       (pp (val rho e)) (run-program rho rest)])))

(define add-* (lambda (x) (string->symbol (string-append (symbol->string x) "*"))))
(define freshen (lambda (used x) (if (memv x used) (freshen used (add-* x)) x)))

(define read-back
  (lambda (used v)
    (pmatch v
      "read-back"
      [;; The only clause that matters
       (CLOS ,rho ,x ,body)
       (let ([y (freshen used x)])
         (let ([neutral-y (N-var y)]
               [used `(,y . ,used)])
           (let ([P (extend rho x neutral-y)])
             `(lambda (,y) ,(read-back used (val P body))))))]
      [(N-var ,x) x]
      [(N-ap ,rator ,rand)
       `(,(read-back used rator) ,(read-back used rand))])))

(define read-back
  ;; The version without renaming
  (lambda (_used v)
    (pmatch v
      "read-back"
      [;; The only clause that matters
       (CLOS ,rho ,x ,body)
       (let ([P (extend rho x `(N-var ,x))])
         `(lambda (,x) ,(read-back '() (val P body))))]
      [(N-var ,x) x]
      [(N-ap ,rator ,rand)
       `(,(read-back '() rator) ,(read-back '() rand))])))

(define norm (lambda (rho e) (read-back '() (val rho e))))

(define run-program
  (lambda (rho exprs)
    (pmatch exprs
      "run-program"
      [() (void)]
      [((define ,x ,e) . ,rest)
       (let ([v (val rho e)])
         (run-program (extend rho x v) rest))]
      [(,e . ,rest)
       (pp (norm rho e))
       (run-program rho rest)])))


;;; Dealing with types
(define type=?
  (lambda (t1 t2)
    (pmatch `(,t1 ,t2)
      "type=?"
      [(Nat Nat) #t]
      [((-> ,A1 ,B1) (-> ,A2 ,B2))
       (and (type=? A1 A2) (type=? B1 B2))]
      [else #f])))

(define type? (lambda (t) (type=? t t)))

(define go (lambda (e) `(go ,e)))
(define stop (lambda (e msg) `(stop ,e ,msg)))
(define-syntax go-on
  (syntax-rules ()
    [(_ () result) result]
    [(_ ([pat0 e0] [pat e] ...) result)
     (pmatch e0
       "go-on"
       [(go ,pat0) (go-on ([pat e] ...) result)]
       [(go ,v) (error 'go-on "Pattern did not match value" v)]
       [(stop expr msg) (stop expr msg)])]))

(define synth
  (lambda (Gamma e)
    (pmatch e
      "synth"
      [;; Type annotations
       (the ,t ,e2)
       (if (not (type? t))
           (stop e (format "Invalid type ~s" t))
           (go-on ([? (check Gamma e2 t)]) (go t)))]
      [;; Recursion on Nat
       (rec ,type ,target ,base ,step)
       (go-on ([target-t (synth Gamma target)]
               [? (if (type=? target-t 'Nat)
                      (go 'ok)
                      (stop target (format "Expected Nat, got ~s" target-t)))]
               [? (check Gamma base type)]
               [? (check Gamma step `(-> Nat (-> ,type ,type)))])
              (go type))]
      [,x (guard (symbol? x)
                 (not (memv x '(the rec lambda zero add1))))
          (cond
           [(assv x Gamma) => (lambda (a) (go (rhs a)))]
           [else (stop x "Variable not found")])]
      [(,rator ,rand)
       (go-on ([rator-t (synth Gamma rator)])
              (pmatch rator-t
                "rator-t"
                [(-> ,A ,B)
                 (go-on ([? (check Gamma rand A)]) (go B))]
                [? (stop rator (format "Not a function type: ~s" rator-t))]))])))

(define check
  (lambda (Gamma e t)
    (pmatch e
      "check"
      [zero
       (if (type=? t 'Nat) (go 'ok)
           (stop e (format "Tried to use ~s for zero" t)))]
      [(add1 ,n)
       (if (type=? t 'Nat)
           (go-on ([? (check Gamma n 'Nat)]) (go 'ok))
           (stop e (format "Tried to use ~s for add1" t)))]
      [(lambda (,x) ,b)
       (pmatch t
         "Arrow"
         [(-> ,A ,B)
          (go-on ([? (check (extend Gamma x A) b B)])
            (go 'ok))]
         [non-arrow
          (stop e (format "Instead of -> type, got ~a" non-arrow))])]
      [;; The harder cases
       ?
       (go-on ([t2 (synth Gamma e)])
         (if (type=? t t2) (go 'ok)
             (stop e (format "Synthesized type ~s where type ~s was expected" t2 t))))])))

(define check-program
  (lambda (Gamma prog)
    (pmatch prog
      "check-program"
      [() (go Gamma)]
      [((define ,x ,e) . ,rest)
       (go-on ([t (synth Gamma e)])
         (check-program (extend Gamma x t) rest))]
      [(,e . ,rest)
       (go-on ([t (synth Gamma e)])
         (begin
           (pp (format "~a has type ~a\n" e t))
           (check-program Gamma rest)))])))

;;; Typed NBE
(define ZERO  (lambda () '(ZERO)))
(define ADD1  (lambda (pred) `(ADD1 ,pred)))
(define NEU   (lambda (type neu) `(NEU ,type ,neu)))
(define N-var (lambda (name) `(N-var ,name)))
(define N-ap  (lambda (rator rand) `(N-ap ,rator ,rand)))
(define N-rec (lambda (type target base step) `(N-rec ,type ,target ,base ,step)))
(define THE   (lambda (type value) `(THE ,type ,value)))
(define norm? (lambda (v) `(THE ,type ,value)))

(define val
  (lambda (rho e)
    (pmatch e
      "val"
      [(the ,type ,expr) (val rho expr)]
      [zero (ZERO)]
      [(add1 ,n) (ADD1 (val rho n))]
      [,x (guard (symbol? x)
                 (not (memv x '(the zero add1 lambda rec))))
          (rhs (assv x rho))]
      [(lambda (,x) ,b) (CLOS rho x b)]
      [(rec ,type ,target ,base ,step)
       (do-rec type (val rho target) (val rho base) (val rho step))]
      [(,rator ,rand)
       (do-ap (val rho rator) (val rho rand))])))

(define do-ap
  (lambda (fun arg)
    (pmatch fun
      "do-ap"
      [(CLOS ,rho ,x ,e) (val (extend rho x arg) e)]
      [(NEU (-> ,A ,B) ,ne) (NEU B (N-ap ne (THE A arg)))])))

(define do-rec
  (lambda (type target base step)
    (pmatch target
      "do-rec"
      [(ZERO) base]
      [(ADD1 ,n)
       (do-ap (do-ap step n)
              (do-rec type n base step))]
      [(NEU Nat ,ne)
       (NEU type
            (N-rec type ne
                   (THE type base)
                   (THE `(→ Nat (→ ,type ,type)) step)))])))

(define read-back
  (lambda (used-names type value)
    (letrec ([read-back-neutral
              (lambda (ne)
                (pmatch ne
                  "read-back-neutral"
                  [(N-var ,x) x]
                  [(N-ap ,fun ,(THE arg-type arg))
                   `(,(read-back-neutral fun)
                     ,(read-back used-names arg-type arg))]
                  [(N-rec ,type ,target ,(THE base-type base) ,(THE step-type step))
                   `(rec ,type
                         ,(read-back-neutral target)
                         ,(read-back used-names base-type base)
                         ,(read-back used-names step-type step))]))])
      (pmatch type
        "read-back"
        [Nat
         (pmatch value
           "Nat"
           [(ZERO) 'zero]
           [(ADD1 ,n) `(add1 ,(read-back used-names 'Nat n))]
           [(NEU ? ,ne) (read-back-neutral used-names ne)])]
        [(-> ,A ,B)
         (let ([x (freshen used-names 'x)])
           `(lambda (,x)
              ,(read-back `(,x . ,used-names)
                          B
                          (do-ap value (NEU A (N-var x))))))]))))
