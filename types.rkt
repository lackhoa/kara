#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out))

(def sym->str symbol->string)

; -------------------------------------------------------
; Macros for Declaring Types
; -------------------------------------------------------
; Constructor declaration for components
(struct Form (path ctor))

(define-syntax-rule (form path e)
  (Form (sym->str 'path) e))

; Symbolic links declaration
(struct SLink (paths))

(define-syntax-rule (== p1 p2 ...)
  (SLink (map sym->str
              (list 'p1 'p2 ...))))

; Type declaration for components
(struct Rec (component type))

(define-syntax-rule (rec component type)
  (Rec (sym->str 'component)
       type))

; Constructors
(struct Ctor (name body))

(define-syntax-rule (ctor name e ...)
  (def name
    (Ctor 'name (list e ...))))

; Types are delayed
(struct Type (body))

(define-syntax-rule (type name e ...)
  (def name
    (Type (delay (list e ...)))))

; Anonymous type
(define-syntax-rule (union e ...)
  (Type (delay (list e ...))))

; -------------------------------------------------------
; Types
; -------------------------------------------------------
; Well-formed Formula
(type wf A B C Implication)

(ctor A) (ctor B) (ctor C)

(ctor Implication
  (rec ante wf)
  (rec ccs wf))

; Logical Entailment
(type entailment AI AK AS AB AC
                 Modus-Ponens)

; => A->A
(ctor AI
  (form ccs Implication)

  (== ccs/ante ccs/csq))

; => (A->B)->A
(ctor AK
  (form ccs     Implication)
  (form ccs/csq Implication)

  (== ccs/ante ccs/csq/csq))

; => (A->(B->C)) -> ((A->B)->(A->C))
(ctor AS
  (form ccs          Implication)
  (form ccs/ante     Implication)
  (form ccs/csq      Implication)
  (form ccs/ante/csq Implication)
  (form ccs/csq/ante Implication)
  (form ccs/csq/csq  Implication)
  ; A
  (== ccs/ante/ante
      ccs/csq/ante/ante
      ccs/csq/csq/ante)
  ; B
  (== ccs/ante/csq/ante
      ccs/csq/ante/csq)
  ; C
  (== ccs/ante/csq/csq
      ccs/csq/csq/csq))

; => (B->C) -> ((A->B) -> (A->C))
(ctor AB
  (form ccs          Implication)
  (form ccs/ante     Implication)
  (form ccs/csq      Implication)
  (form ccs/csq/ante Implication)
  (form ccs/csq/csq  Implication)
  ; A
  (== ccs/csq/ante/ante
      ccs/csq/csq/ante)
  ; B
  (== ccs/ante/ante
      ccs/csq/ante/csq)
  ; C
  (== ccs/ante/csq
      ccs/csq/csq/csq))

; => (A->(B->C)) -> (B->(A->C))
(ctor AC
  (form ccs          Implication)
  (form ccs/ante     Implication)
  (form ccs/csq      Implication)
  (form ccs/ante/csq Implication)
  (form ccs/csq/csq  Implication)
  ; A
  (== ccs/ante/ante
      ccs/csq/csq/ante)
  ; B
  (== ccs/ante/csq/ante
      ccs/csq/ante)
  ; C
  (== ccs/ante/csq/csq
      ccs/csq/csq/csq))

; A, A->B => B
(ctor Modus-Ponens
  (rec ?=>a    entailment)
  (rec ?=>a->b entailment)

  (form ?=>a->b/ccs
        Implication)
  ; A
  (== ?=>a->b/ccs/ante
      ?=>a/ccs)
  ; B
  (== ccs
      ?=>a->b/ccs/csq))
; Note: the conclusion (ccs) is implicit in the last link.
