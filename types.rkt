#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out))

(def sym->str symbol->string)

; -------------------------------------------------------
; Some macros to start things off
; -------------------------------------------------------
; Constructor declaration for components
(define-syntax-rule (form path e)
  (tag 'form
       (cons (sym->str 'path)
             e)))

; Symbolic links declaration
(define-syntax-rule (== path1 path2)
  (tag 'slink
       (cons (sym->str 'path1)
             (sym->str 'path2))))

; Type declaration for components
(define-syntax-rule (rec component type)
  (tag 'rec
       (cons (sym->str 'component)
             type)))

; Constructors
(define-syntax-rule (ctor name e ...)
  (def name (list 'name e ...)))

; Types are delayed
(define-syntax-rule (type name e ...)
  (def name (delay (list e ...))))

; Anonymous type
(define-syntax-rule (union e ...)
  (delay (list e ...)))

; -------------------------------------------------------
; These are the types
; -------------------------------------------------------
(type wff A B C Implication)

(ctor A) (ctor B) (ctor C)

(ctor Implication
  (rec ante wff)
  (rec conclusion wff))

(type entailment I-Combinator
                 K-Combinator
                 Chain
                 Modus-Ponens)

(ctor I-Combinator
  (form conclusion Implication)
  (==   conclusion/ante conclusion/csq))

(ctor K-Combinator
  (form conclusion      Implication)
  (form conclusion/csq  Implication)
  (==   conclusion/ante conclusion/csq/csq))

(ctor Chain
  (rec a=>b
       (union I-Combinator K-Combinator))
  (rec b=>c            entailment)
  (rec conclusion      wff)
  (== a=>b/conclusion
      b=>c/prem))

(ctor Modus-Ponens
  (rec  a->b       entailment)
  (rec  a          entailment)
  (rec  conclusion wff)

  (form a->b/conclusion Implication)

  (== a->b/conclusion/ante
      a/conclusion)
  (== conclusion
      a->b/conclusion/csq))
