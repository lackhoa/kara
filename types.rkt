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

; Symbol: construtor with just a representation.
(define-syntax-rule (Symbol s)
  (Ctor [macro-repr (s)] null null null))

; -------------------------------------------------------
; Type Definitions
; -------------------------------------------------------
; Well-formed Formula
(def wf (macro-union A-Sym
                     B-Sym
                     C-Sym
                     Implication))

(def A-Sym (Symbol A))
(def B-Sym (Symbol B))
(def C-Sym (Symbol C))

(def-ctor Implication
  [macro-repr (-> ante csq)]
  [macro-recs (ante wf) (csq wf)]
  null
  null)

; Logical Entailment: only conclusion (ccs) is necessary.
(def entailment
  (macro-union AI AK AS AB AC MP))

; => A->A
(def-ctor AI
  [macro-repr (AI=> ccs)]

  null

  [macro-forms
   (ccs  Implication)]

  [macro-links
   (ccs_ante ccs_csq)])

; => (A->B)->A
(def-ctor AK
  [macro-repr (AK=> ccs)]

  null

  [macro-forms
   (ccs      Implication)
   (ccs_csq  Implication)]

  [macro-links
   (ccs_ante ccs_csq_csq)])

; => (A->(B->C)) -> ((A->B)->(A->C))
(def-ctor AS
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
(def-ctor AB
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
(def-ctor AC
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
(def-ctor MP
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
