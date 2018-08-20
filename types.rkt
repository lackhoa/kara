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

; Constructors
(struct Ctor (repr recs forms slinks)
  #:methods gen:custom-write
  [(define (write-proc arg-ctor port mode)
     (display (Ctor-repr arg-ctor) port))])

; Short-hand for defining new construtors
(define-syntax-rule (def-ctor name repr recs forms slinks)
  (def name
    (Ctor repr recs forms slinks)))

; Form declaration
(struct Form (path ctor))

(define-syntax-rule (single-form path c)
  (Form (path-proc 'path) c))

(define-syntax-rule (macro-forms (path c) ...)
  (list (single-form path c) ...))

; Link declaration
(struct SLink (path1 path2))

(define-syntax single-link
  (syntax-rules ()
    [(_ p1 p2)
     (list
       (SLink (path-proc 'p1)
              (path-proc 'p2)))]

    [(_ p1 p2 rest ...)
     (cons (SLink (path-proc 'p1)
                  (path-proc 'p2))
           (single-link p2 rest ...))]))

(define-syntax-rule (macro-links (p1 p2 rest ...) ...)
  (append (single-link p1 p2 rest ...) ...))

; Recursive component declaration
(struct Rec (role type))

(define-syntax-rule (single-rec role type)
  (Rec 'role type))

(define-syntax-rule (macro-recs (role type) ...)
  (list (single-rec role type) ...))

; Representation
(struct Repr (leader paths)
  #:methods gen:custom-write
  [(define (write-proc arg-repr port mode)
     (display (cons (Repr-leader arg-repr)
                    (Repr-paths arg-repr))
              port))])

(define-syntax macro-repr
  (syntax-rules ()
    [(_ (leader roles ...))
     (Repr 'leader (list 'roles ...))]))

; Union: a delayed list of constructors.
(struct Union (ctors))

(define-syntax-rule (macro-union ctors ...)
  (Union (delay (list ctors ...))))

; Symbols: leaf construtors (or terminals).
(define-syntax-rule (Sym s)
  (Ctor [macro-repr (s)] null null null))

; Defining axioms.
(define-syntax-rule (Axiom name e ...)
  (begin
    (def-ctor name e ...)
    (cons! name entailment)))


; -------------------------------------------------------
; Deduction System
; -------------------------------------------------------
; Well-formed Formula
(def wf
  (macro-union Atom Implication))

; Really, well-formed-formulas can be anything.
(def Atom 'ANY)

; Symbols for testing. We aren't limited to working with symbols.
(def A (Sym A))
(def B (Sym B))
(def C (Sym C))

(def-ctor Implication
  [macro-repr (-> ante csq)]
  [macro-recs (ante wf) (csq wf)]
  null null)

; Logical Entailment: only conclusion (ccs) is necessary.
(def entailment
  (Union (list)))

; => A->A
(Axiom AI
  [macro-repr (AI=> ccs)]

  null

  [macro-forms
   (ccs  Implication)]

  [macro-links
   (ccs_ante ccs_csq)])

; => (A->B)->A
(Axiom AK
  [macro-repr (AK=> ccs)]

  null

  [macro-forms
   (ccs      Implication)
   (ccs_csq  Implication)]

  [macro-links
   (ccs_ante ccs_csq_csq)])

; => (A->(B->C)) -> ((A->B)->(A->C))
(Axiom AS
  [macro-repr (AS=> ccs)]

  null

  [macro-forms
   (ccs          Implication)
   (ccs_ante     Implication)
   (ccs_csq      Implication)
   (ccs_ante_csq Implication)
   (ccs_csq_ante Implication)
   (ccs_csq_csq  Implication)]

  ; A
  [macro-links
   (ccs_ante_ante
    ccs_csq_ante_ante
    ccs_csq_csq_ante)
   ; B
   (ccs_ante_csq_ante
    ccs_csq_ante_csq)
   ; C
   (ccs_ante_csq_csq
    ccs_csq_csq_csq)])

; => (B->C) -> ((A->B) -> (A->C))
(Axiom AB
  [macro-repr (AB=> ccs)]

  null

  [macro-forms
   (ccs          Implication)
   (ccs_ante     Implication)
   (ccs_csq      Implication)
   (ccs_csq_ante Implication)
   (ccs_csq_csq  Implication)]

  [macro-links
   ; A
   (ccs_csq_ante_ante ccs_csq_csq_ante)
   ; B
   (ccs_ante_ante ccs_csq_ante_csq)
   ; C
   (ccs_ante_csq ccs_csq_csq_csq)])

; => (A->(B->C)) -> (B->(A->C))
(Axiom AC
  [macro-repr (AC=> ccs)]

  null

  [macro-forms
   (ccs          Implication)
   (ccs_ante     Implication)
   (ccs_csq      Implication)
   (ccs_ante_csq Implication)
   (ccs_csq_csq  Implication)]

  [macro-links
   ; A
   (ccs_ante_ante ccs_csq_csq_ante)
   ; B
   (ccs_ante_csq_ante ccs_csq_ante)
   ; C
   (ccs_ante_csq_csq ccs_csq_csq_csq)])

; A, A->B => B
(Axiom MP
  [macro-repr (MP=> ?=>a->b ?=>a ccs)]

  [macro-recs
   (?=>a     entailment)
   (?=>a->b  entailment)]

  [macro-forms
   (?=>a->b_ccs  Implication)]

  [macro-links
   ; A
   (?=>a->b_ccs_ante  ?=>a_ccs)
   ; B
   (ccs  ?=>a->b_ccs_csq)])
; Note: the conclusion (ccs) is implicit in the last link.

; -------------------------------------------------------
; Equality
; -------------------------------------------------------
; I think I don't know anything about type.
(def Eq
  (Ctor
    [macro-repr (= lhs rhs)]
    ; Yeah, the two sides can be anything.
    [macro-recs
      (lhs 'ANY)
      (rhs 'ANY)]
    null null))

; x = x {any type}
(Axiom Reflexivity
  [macro-repr (Ref=> ccs)]

  null

  [macro-forms
    (ccs Eq)]

  [macro-links (ccs_lhs ccs_rhs)])

; x = y -> y = x {any type}
(Axiom Commutativity
  [macro-repr (Comm=> ccs)]

  null

  [macro-forms
    (ccs      Implication)
    (ccs_ante Eq)
    (ccs_csq  Eq)]

  [macro-links
    (ccs_ante_lhs ccs_ante_rhs)  ; x
    (ccs_ante_rhs ccs_csq_lhs)]) ; y

; (x = y) -> ((y = z) -> (x = z))
(Axiom Transitivity
  [macro-repr (Tran=> ccs)]

  null

  [macro-forms
    (ccs          Implication)
    (ccs_ante     Eq)
    (ccs_csq      Implication)
    (ccs_csq_ante Eq)
    (ccs_csq_csq  Eq)]

  [macro-links
    (ccs_ante_lhs     ccs_csq_csq_lhs)   ; x
    (ccs_ante_rhs     ccs_csq_ante_lhs)  ; y
    (ccs_csq_ante_rhs ccs_csq_csq_rhs)]) ; z


; (x = y) ->
; ((z = t) -> ((x -> z) = (y -> t)))
(Axiom (Eq-Wf)
  [macro-repr (Eq-Wf=> ccs)]

  null

  [macro-forms
    (ccs             Implication)
    (ccs_ante        Eq)
    (ccs_csq         Implication)
    (ccs_csq_ante    Eq)
    (ccs_csq_csq     Eq)
    (ccs_csq_csq_lhs Implication)
    (ccs_csq_csq_rhs Implication)]

  [macro-links
    (ccs_ante_lhs     ccs_csq_csq_lhs_ante)  ; x
    (ccs_ante_rhs     ccs_csq_csq_rhs_ante)  ; y
    (ccs_csq_ante_lhs ccs_csq_csq_lhs_csq)   ; z
    (ccs_csq_ante_rhs ccs_csq_csq_rhs_csq)]) ; t

; (x = y) -> (x -> y) {any type}
(Axiom I9
  [macro-repr (I9=> ccs)]

  null

  [macro-forms
    (ccs      Implication)
    (ccs_ante Eq)
    (ccs_csq  Implication)]

  [macro-links
    (ccs_ante_lhs ccs_csq_ante)
    (ccs_ante_rhs ccs_csq_csq)])

; -------------------------------------------------------
; Category Theory
; -------------------------------------------------------

(def Object 'ANY)

; The morphism type: source and target are common.
(def mor
  (macro-union Atomic-Map Comp Iden))

(def-ctor Atomic-Map
  [macro-repr (-> name src targ)]
  null
  [macro-recs
   (name 'ANY)
   (src  Object)
   (targ Object)]
  null)

(def-ctor Comp
  [macro-repr (∘ g f)]
  [macro-recs
    (g mor)
    (f mor)]
  null
  [macro-links
    (src  f_src)
    (targ g_targ)])

(def-ctor Iden
  [macro-repr (|1| src)]
  null
  null
  [macro-links (src targ)])

; f: A -> B => 1B ∘ f = f
(Axiom Iden-Axiom1
  [macro-repr (IM1=> ccs)]

  [macro-recs (rhs mor)]

  [macro-forms
    (ccs       Eq)
    (ccs_lhs   Comp)
    (ccs_lhs_g Iden)]

  [macro-links
    (ccs_lhs_f     rhs)
    (ccs_lhs_g_src rhs_targ)])

; => g ∘ 1A = g (g: A -> B)
(Axiom Iden-Axiom2
  [macro-repr (IM2=> ccs)]

  [macro-recs (rhs mor)]

  [macro-forms
    (ccs       Eq)
    (ccs_lhs   Comp)
    (ccs_lhs_f Iden)]

  [macro-links
    (ccs_lhs_g     rhs)
    (ccs_lhs_f_src rhs_src)])

; => (h ∘ g) ∘ f = h ∘ (g ∘ f)
(Axiom Comp-Assoc
  [macro-repr (Assoc=> ccs)]
  [macro-recs
    (ccs_lhs_f mor)
    (ccs_rhs_g mor)]
  [macro-forms
    (ccs        Eq)
    (ccs_lhs_g Comp)
    (ccs_rhs_f Comp)]
  [macro-links
    (ccs_lhs_f   ccs_rhs_f_f) ; f
    (ccs_lhs_g_f ccs_rhs_f_g) ; g
    (ccs_lhs_g_g ccs_rhs_g)]) ; h

; (f = g) -> ((h = i) -> (f ∘ h = g ∘ i))
(Axiom Comp-Eq
  [macro-repr (Comp-Eq=> ccs)]

  null

  [macro-forms
    (ccs_ante        Eq)
    (ccs_csq         Implication)
    (ccs_csq_ante    Eq)
    (ccs_csq_csq     Eq)
    (ccs_csq_csq_lhs Comp)
    (ccs_csq_csq_rhs Comp)]

  [macro-links
    (ccs_ante_lhs     ccs_csq_csq_lhs_g)   ; f
    (ccs_ante_rhs     ccs_csq_csq_rhs_g)   ; g
    (ccs_csq_ante_lhs ccs_csq_csq_lhs_f)   ; h
    (ccs_csq_ante_rhs ccs_csq_csq_rhs_f)]) ; i
