;; Rules & Notes & Conventions & Stuffs
#|
Keep variable names and constants' name separate
A state is: assumptions + goals
Each state can either add new assumption, or remove the top-most goals
|#

;; Works with the canonical mk version

;;; Macros & helpers
(define-syntax forall
  (syntax-rules ()
    [(_ () body) 'body]
    [(_ (id id* ...) body)
     `(forall id ,(forall (id* ...) body))]))
(define my-rhs cadr)

(define-record proof (ass goals))

;; Inference rules
(define ind
  (lambda (P)
    (act
     (prove (inst P 0))
     (prove `(forall m (-> ,(inst P 'm)
                     ,(inst P '(S m))))))))

(define ->2
  (lambda (imp ante)
    (pmatch imp
      [(-> ,ante^ ,conse)
       (guard (equal? ante ante^))
       (assume conse)]
      [else (error '->2 "Can't apply" imp ante conse)])))

(define-syntax ->
  (syntax-rules ()
    [(_ imp ante)
     (->2 imp ante)]
    [(_ imp ante0 ante1 ante* ... conse)
     (act
      (->2 imp ante0)
      (-> ante1 ante* ... conse))]))

;;; Axioms and Schemas
(define refl '(forall X (= X X)))

(define rwl
  (forall (L L* R)
     (-> (= L R) (-> (= L L*) (= L* R)))))
(define rwr
  (forall (L R R*)
     (-> (= L R) (-> (= R R*) (= L R*)))))

(define arity-table '([S 1] [+ 2]))

(define inj
  (lambda (func-sym)
    (let ([arity (my-rhs (assq func-sym arity-table))])
      (let ([Xs (gen-vars "X" arity)]
            [Ys (gen-vars "Y" arity)])
        (let loop ([i arity] [X* Xs] [Y* Ys])
          (cond
           [(= i 0) `(= (,func-sym . ,Xs)
                        (,func-sym . ,Ys))]
           [else
            `(-> (= ,(car X*) ,(car Y*))
                ,(loop (- i 1) (cdr X*) (cdr Y*)))]))))))

(define gen-vars
  ;; base-name is string
  (lambda (base-name n)
    (let gen-vars ([i n])
      (if (= i 0) '()
          `(,(string->symbol (string-append base-name (number->string i)))
            .
            ,(gen-vars (- i 1)))))))

(define plus0 (forall (X) (= (+ 0 X) X)))

(define plus-S (forall (X Y) (= (+ (S X) Y)
                           (S (+ X Y)))))

(define plus0r (forall (X) (= (+ X 0) X)))

(define =sym (forall (X Y) (-> (= X Y) (= Y X))))
