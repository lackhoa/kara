(load "lookup.ss")
(define CLOS  (lambda (env var body) `(CLOS ,env ,var ,body)))
(define N-var (lambda (name) `(N-var ,name)))
(define N-ap  (lambda (rator rand) `(N-ap ,rator ,rand)))

(define val
  (lambda (rho e)
    (pmatch e
      [(lambda (,x) ,b) (CLOS rho x b)]
      [,x (guard (symbol? x))
          (cond
           [(assv x rho) => rhs]
           [else (error 'val "Unknown variable" x)])]
      [(,rator ,rand) (do-ap (val rho rator) (val rho rand))])))

(define do-ap
  (lambda (fun arg)
    (pmatch fun
      [(CLOS ,rho ,x ,b) (val (extend rho x arg) b)]
      [;; Will only be met if called from read-back
       else `(N-ap ,fun ,arg)])))

(define run-program
  (lambda (rho exprs)
    (pmatch exprs
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
