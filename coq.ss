#| Rules & Notes & Conventions & Stuffs
Context: Assets x Goal
Assets: [Formula]
Goal: Formula
State: [Context]
Local context: The first context in the state
Action: State -> State
:- c a ... adds (-> c a ...) itself to the local asset (the symbol change is to distinguish consequent from implication)
|#

;;; Macros & helpers
(define for
  (lambda (f)
    (let ([vs (remove-duplicates (all-vars f))])
      (let loop ([vs vs])
        (cond
         [(null? vs) f]
         [else `(forall ,(car vs) ,(loop (cdr vs)))])))))

(define all-vars
  (lambda (f)
    (cond
     [(var? f) `(,f)]
     [(pair? f) (append (all-vars (car f))
                        (all-vars (cdr f)))]
     [else '()])))

(define remove-duplicates
  (lambda (ls)
    (cond
     [(null? ls) '()]
     [(let ([a (car ls)]
            [d (cdr ls)])
        (cond
         [(memq a d) (remove-duplicates d)]
         [else `(,a . ,(remove-duplicates d))]))])))

;;; Definitions
(define-record ctx (a g))

(define var?
  (lambda (t)
    (and (symbol? t)
         (char-upper-case? (string-ref (symbol->string t) 0)))))

(define init-env (lambda (g) `(,(make-ctx '() g))))

(define-syntax go
  (syntax-rules ()
    [(_) (lambda (x) x)]
    [(_ f f* ...) (lambda (x)
                    ((go f* ...) (f x)))]))

(define-syntax prove
  (lambda (g . steps)
    ((go steps) (init-env g))))

;;; Inference rules (actions)
(define mp
  (lambda (imp ante)
    (lambda (env)
      (pmatch imp
        [(-> ,ante^ ,conse)
         (guard (equal? ante ante^))
         (gain conse env)]))))

(define intro
  (lambda (env)
    (let ([a (current-a env)]
          [g (current-g env)])
      (pmatch f
        [(forall ,X ,f)
         (fresh (Y) (gain (subst X Y f) env))]
        [(-> ,ante ,conse)
         `(,(make-ctx a conse) . ,(cdr env))]))))

(define gain
  (lambda (f env)
    (let ([a (current-a env)]
          [g (current-g env)])
      `(,(make-ctx `(,f . ,a) g) . ,(cdr env)))))

;;; Axioms
(define ind
  ;; schema: state -> formulae
  (lambda (v P)
    (-> (inst P v 0)
       (-> `(forall M (-> ,(inst P v 'M)
                   ,(inst P v '(s M))))
          P))))

(define arity-table '([S 1] [+ 2]))

;; Equality
(define refl (for '(= X X)))
(define rwl  (for '(-> (= L R) (-> (= L L*) (= L* R)))))
(define rwr  (for '(-> (= L R) (-> (= R R*) (= L R*)))))
(define =sym (for '(-> (= X Y) (= Y X))))

;; Arithmetic
(define plus0  (for '(= (+ 0 X) X)))
(define plus-S (for '(= (+ (s X) Y) (s (+ X Y)))))
(define plus0r (for '(= (+ X 0) X)))


#!eof
