#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out))

;; -------------------------------------------------------
;; Macros for Type Declaration
;; -------------------------------------------------------

;; Constructors
(struct Ctor (leader recs forms slinks)
  #:methods gen:custom-write
  [(define (write-proc ctor port mode)
     (display (Ctor-leader ctor) port))])

;; union: a delayed list of constructors.
(define-syntax-rule (union blah ...)
  (stream blah ...))

;; Symbols: leaf construtors (or terminals).
(define-syntax-rule (Sym s)
  (Ctor 's null null null))

(def recs list)

(define-syntax forms
  (syntax-rules ()
    [(_)  (list)]

    [(_ (path ctor) rest ...)
     (cons (cons 'path ctor)
           (forms rest ...))]))

(define-syntax links
  (syntax-rules ()
    [(_)  (list)]

    [(_ (paths ...) rest ...)
     (cons (list 'paths ...)
           (links rest ...))]))

;; -------------------------------------------------------
;; Deduction System
;; -------------------------------------------------------
;; Well-formed Formula: A, B, and C are just for testing.
(def wf
  (union Implication))

;; Symbols for testing.
(def A (Sym A)) (def B (Sym B)) (def C (Sym C))
(def X (Sym X)) (def Y (Sym Y)) (def Z (Sym Z))
(def W (Sym W)) (def J (Sym J)) (def K (Sym K))
(def V (Sym V))
(def T (Sym T)) (def S (Sym S)) (def P (Sym P))
(def Q (Sym Q)) (def R (Sym R)) (def D (Sym D))
(def G (Sym G)) (def F (Sym F)) (def L (Sym L))

(def Implication
  (let ([m (new mole%)])
    (update-macro
     m
     ([] '->))
    m))

;; Logical Entailment: only conclusion is necessary.
(def entailment
  (union AI AK AS AB MP))

(def AI
  ;; => A->A
  (let ([m (new mole%)])
    (update-macro
     m
     ([]  'AI=>)
     ([0] Implication))

    (sync-macro
     m
     ([0 0] [0 1]))

    m))

(def AK
  ;; => A -> (B->A)
  (let ([m (new mole%)])
    (update-macro
     m
     ([]  'AK=>)
     ([0] Implication))

    (sync-macro
     m
     ([0 0] [0 1]))

    m))

(def AK
  (Ctor 'AK=>
        (recs)
        (forms ([0]   Implication)
               ([0 1] Implication))
        (links ([0 0] [0 1 1]))))

(def AS
  ;; (=> (-> (-> A
  ;;             (-> B C))
  ;;         (-> (-> A B)
  ;;             (-> A C))))
  (Ctor 'AS=>
        (recs)
        (forms ([0]     Implication)
               ([0 0]   Implication)
               ([0 0 1] Implication)  ; (-> B C)
               ([0 1]   Implication)
               ([0 1 0] Implication)  ; (-> A B)
               ([0 1 1] Implication)) ; (-> A C)

        (links ([0 0 0]   [0 1 0 0] [0 1 1 0])
               ([0 0 1 0] [0 1 0 1])
               ([0 0 1 1] [0 1 1 1]))))

(def AB
  ;; (=> (-> (-> B C)
  ;;         (-> (-> A B)
  ;;             (-> A C))))
  (Ctor 'AB=>
        (recs)
        (forms ([0]     Implication)
               ([0 0]   Implication)  ; (-> B C)
               ([0 1]   Implication)
               ([0 1 0] Implication)  ; (-> A B)
               ([0 1 1] Implication)) ; (-> A C)

        (links ([0 1 0 0] [0 1 1 0])
               ([0 0 0]   [0 1 0 1])
               ([0 1 1 1] [0 0 1]))))

(def MP
  ;; (=> A
  ;;    (=> (-> B A))
  ;;    (=> B))
  (Ctor 'MP=>
        (recs wf
              entailment
              entailment)

        (forms ([1 0] Implication))  ; (-> B A)

        (links ([0]     [1 0 1])  ; A
               ([1 0 0] [2 0])))) ; B
