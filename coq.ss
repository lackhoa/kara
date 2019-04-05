;; Rules & Notes & Conventions & Stuffs
#|
Context is like environment: each implication subgoal can add its own assumptions to the context
|#

;; Works with the canonical mk version

;;; Macros & helpers
(define-record context (S ))
(define my-rhs cadr)

;; Inference rules
(define ind
  (lambda (x)
    (lambda (P)
      ;; P is current goal
      (spawn (;; No assumptions
              )
             (;; Prove these
              (inst P x 0)
              (letv (m) (-> ,(inst P x m)
                           ,(inst P `(S ,m)))))))))

(define-syntax ->
  (syntax-rules ()
    [(_ imp ante)
     (->2 imp ante)]
    [(_ imp ante0 ante1 ante* ... conse)
     (act
      (->2 imp ante0)
      (-> ante1 ante* ... conse))]))

(define ->2
  (lambda (imp ante)
    (pmatch imp
      [(-> ,ante^ ,conse)
       (guard (equal? ante ante^))
       (derive conse)]
      [else (error '->2 "Can't apply" imp ante)])))

;;; Axioms and Schemas
(define inj
  (lambda (func-sym)
    (let ([arity (my-rhs (assq func-sym arity-table))])
      (let loop ([i arity] [Xs '()] [Ys '()])
        (cond
         [(= i 0) `(= (,func-sym . ,Xs)
                      (,func-sym . ,Ys))]
         [else
          (new (X Y)
               `(-> (= ,X ,Y)
                   ,(loop (- i 1) `(,X . ,Xs) `(,Y . ,Ys))))])))))

(define arity-table '([S 1] [+ 2]))
(define refl (letv (X) `(= ,X ,X)))
(define rwl (letv (L L* R) `(-> (= ,L ,R) (-> (= ,L ,L*) (= ,L* ,R)))))
(define rwr (letv (L R R*) `(-> (= ,L ,R) (-> (= ,R ,R*) (= ,L ,R*)))))
(define =sym (letv (X Y) `(-> (= ,X ,Y) (= ,Y ,X))))





#!eof
;; Arithmetic
(define plus0 (letv (X) `(= (+ 0 ,X) X)))

(define plus-S (= (+ (S X) Y)
                  (S (+ X Y))))

(define plus0r (= (+ X 0) X))
