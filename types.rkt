#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out))

; Turn a symbolic path into a normalized path-list.
(def (path-proc path)
  (map string->symbol
       (string-split (symbol->string path)
                     "_")))

; -------------------------------------------------------
; Macros for Type Declaration
; -------------------------------------------------------
; Constructor declaration for components.
(struct Form (path ctor))

(define-syntax-rule (form path e)
  (Form (path-proc 'path) e))

; Symbolic links declaration.
(struct SLink (p1 p2))

(define-syntax --
  (syntax-rules ()
    [(_ p1 p2)
     (list (SLink (path-proc 'p1)
                  (path-proc 'p2)))]
    [(_ p1 p2 rest ...)
     (cons (SLink (path-proc 'p1)
                  (path-proc 'p2))
           (-- p2 rest ...))]))

; Type declaration for components.
(struct Rec (role type))

(define-syntax-rule (rec role type)
  (Rec 'role type))

; Constructors: body is a list of Form, Rec, and Slinks.
(struct Ctor (name body)
        #:methods gen:custom-write
        [(def (write-proc Ctor port mode)
           (display (Ctor-name Ctor) port))])

(define-syntax-rule (ctor name e ...)
  (def name
    (Ctor 'name (flatten (list e ...)))))  ; `flatten` since SLinks may be grouped

; Types are delayed list of constructors.
(struct Type (body))

(define-syntax-rule (type name e ...)
  (def name
    (Type (delay (list e ...)))))

; Anonymous type
(define-syntax-rule (union e ...)
  (Type (delay (list e ...))))

; -------------------------------------------------------
; Type Definitions
; -------------------------------------------------------
; Well-formed Formula
(type wf A B C Implication)

(ctor A) (ctor B) (ctor C)

(ctor Implication
  (rec ante wf)
  (rec ccs wf))

; Logical Entailment: only conclusion (ccs) is necessary.
(type entailment
  AI AK AS AB AC MP)

; => A->A
(ctor AI
  (form ccs  Implication)

  (-- ccs_ante ccs_csq))

; => (A->B)->A
(ctor AK
  (form ccs     Implication)
  (form ccs_csq Implication)

  (-- ccs_ante ccs_csq_csq))

; => (A->(B->C)) -> ((A->B)->(A->C))
(ctor AS
  (form ccs          Implication)
  (form ccs_ante     Implication)
  (form ccs_csq      Implication)
  (form ccs_ante_csq Implication)
  (form ccs_csq_ante Implication)
  (form ccs_csq_csq  Implication)
  ; A
  (-- ccs_ante_ante
      ccs_csq_ante_ante
      ccs_csq_csq_ante)
  ; B
  (-- ccs_ante_csq_ante
      ccs_csq_ante_csq)
  ; C
  (-- ccs_ante_csq_csq
      ccs_csq_csq_csq))

; => (B->C) -> ((A->B) -> (A->C))
(ctor AB
  (form ccs          Implication)
  (form ccs_ante     Implication)
  (form ccs_csq      Implication)
  (form ccs_csq_ante Implication)
  (form ccs_csq_csq  Implication)
  ; A
  (-- ccs_csq_ante_ante
      ccs_csq_csq_ante)
  ; B
  (-- ccs_ante_ante
      ccs_csq_ante_csq)
  ; C
  (-- ccs_ante_csq
      ccs_csq_csq_csq))

; => (A->(B->C)) -> (B->(A->C))
(ctor AC
  (form ccs          Implication)
  (form ccs_ante     Implication)
  (form ccs_csq      Implication)
  (form ccs_ante_csq Implication)
  (form ccs_csq_csq  Implication)
  ; A
  (-- ccs_ante_ante
      ccs_csq_csq_ante)
  ; B
  (-- ccs_ante_csq_ante
      ccs_csq_ante)
  ; C
  (-- ccs_ante_csq_csq
      ccs_csq_csq_csq))

; A, A->B => B
(ctor MP
  (rec ?=>a    entailment)
  (rec ?=>a->b entailment)

  (form ?=>a->b_ccs  Implication)
  ; A
  (-- ?=>a->b_ccs_ante
      ?=>a_ccs)
  ; B
  (-- ccs
      ?=>a->b_ccs_csq))
; Note: the conclusion (ccs) is implicit in the last link.
