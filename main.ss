;;; Help functions
(define pp pretty-print)
(define ppl (lambda (ls) (for-each pp ls)))

(define repeat-func
  (lambda (i f)
    (unless (= i 0) (f) (repeat-func (- i 1) f))))
(define-syntax repeat
  (syntax-rules ()
    [(_ i e)
     (repeat-func i (lambda () e))]))

(define-syntax reflect
  ;; Goal for debugging
  (syntax-rules ()
    [(_ msg x)
     (project (x) (begin (pp msg) (pp x) succeed))]))

;;; miniKanren
;; (load "faster-miniKanren/mk-vicare.scm")
;; (load "faster-miniKanren/mk.scm")
;; (load "faster-miniKanren/matche.scm")
;; (load "faster-miniKanren/numbers.scm")
(load "miniKanren/mk.scm")
;; (load "micro.ss")

;;; Other "libraries"
(load "reif.ss")
(load "pmatch.scm")

;;; The main program
;; (load "faster-miniKanren/full-interp.scm")
;; (load "full-interp.ss")
;; (load "test-reif.ss")
;; (load "test-full-interp.ss")
;; (load "test-compiler.ss")
;; (load "test-net.ss")
;; (load "test-coq.ss")
;; (load "test-fw.ss")
;; (load "lambda.ss")
;; (load "lambda2.ss")

(define lookupo
  (lambda (x env t bound?)
    (conde
     [(== '() env ) (== #f bound?)]
     [(fresh (y b rest)
        (== `((,y ,b ) . ,rest) env )
        ( conde
          [(== y x ) (== #t bound?) (== b t )]
          [(=/= y x ) (lookupo x rest t bound?)]))])))

(define case2o
  (lambda (x y env out)
    (conde
     [(lookupo x env out #t)]
     [(lookupo x env '? #f) (lookupo y env out #t)]
     [(lookupo x env '? #f) (lookupo y env '? #f)
      (== #f out)])))

(define lookup
  (lambda (x env)
    (cond
     [(null? env) #f]
     [else
      (let ([a (car env)])
        (cond
         [(eq? x (lhs a)) a]
         [else (lookup x (cdr env))]))])))

(define lookupt
  (lambda (x env t)
    (lambda (bound?)
      (conde
       [(== '() env)
        (== #f bound?) (== t 'unbound)]
       [(fresh (y b rest)
          (== `((,y ,b) . ,rest) env)
          (condo
           [(==t y x) (== #t bound?) (== b t)]
           [else
            ((lookupt x rest t) bound?)]))]))))


(define case1
  (lambda (x env)
    (cond
     [(lookup x env) => rhs]
     [else #f])))

(define case1o
  (lambda (x env out)
    (conde
     [(lookupo x env out #t)]
     [(lookupo x env 'unbound #f) (== #f out)])))

(define case2o
  (lambda (x y env)
    (lambda (out)
      (condo
       [(lookupt x env out) succeed]
       [(lookupt y env out) succeed]
       [else (== #f out)]))))

(pp
 (run* (x y)
   (condo
    [(==t 'a x) (== 'A y)]
    [(==t 'b x) (== 'B y)]
    [(==t 'c x) (== 'C y)])))

#!eof
(define case2
  (lambda (x y env)
    (cond
     [(lookup x env) => rhs]
     [(lookup y env) => rhs]
     [else #f])))

(define case2o
  (lambda (x y env)
    (lambda (out)
      (condo
       [(lookupt x env out) succeed]
       [(lookupt y env out) succeed]
       [else (== #f out)]))))

(fresh (x y)
  (condo
   [(==t 'a x) (== 'A y)]
   [(==t 'b x) (== 'B y)]
   [(==t 'c x) (== 'C y)]))

(fresh (x y)
  (conde
   [(== 'a x) (== 'A y)]
   [(=/= 'a x) (== 'b x) (== 'B y)]
   [(=/= 'a x) (=/= 'b x) (== 'c x) (== 'C y)]))

((_.0 _.0 _.1)
 ((_.0 _.1 _.1) (=/= ((_.0 _.1))))
 ((_.0 _.1 _.0) (=/= ((_.0 _.1)))))
