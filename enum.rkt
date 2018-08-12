#lang racket
(require "lang/kara.rkt"
         racket/generic
         "engine.rkt"
         "gen_robin.rkt"
         "mole.rkt"
         "types.rkt")

(struct Target (path type))

; start enumerating with the root as the target
(def (kick-start mole [root-type 'NOT-NEEDED])
  (enum mole
        (QTars (list (Target root-type)))))

(define-generics targets
  ; Returns the next target
  (tar-next targets)
  ; Discard the next target (returns same interface)
  (tar-rest targets)
  (tar-add targets new-targets))

(define-struct QTars (lineup)
  #:methods gen:targets
  [(def (tar-next q)
     (car (QTars-lineup q)))

   (def (tar-rest q)
     (QTars (cdr (QTars-lineup q))))

   (def (tar-add q new-targets)
     (QTars (append (QTars-lineup q)
                    new-targets)))])

; Returns: a generator of complete molecules
(def (enum mole targets)
  (generator ()
    (if (null? targets)
        (begin (yield mole)
               (yield 'DONE))
      (let* ([t-first (tar-next targets)]
             [t-rest  (tar-rest targets)]
             [result  (advance mole t-first)])
        (if (eq? result 'INCONSISTENT)
            'DONE
          ; Multitask all the different branches
          (gen-robin
            (map (lam (result-iter)
                   (let ([new-mole    (car result-iter)]
                         [new-targets (cdr result-iter)])
                     (enum new-mole targets)))
                  result)))))))

; Output: a list of molecules
(def (advance mole target)
  (let* ([tpath (Target-path target)]
         [ttype (Target-type target)]
         [val   ((mole 'ref) tpath)])
    (if (eq? val 'NOT-FOUND)
        (remove 'INCONSISTENT
          (map (lam (ttype-iter)
                 (work-with-ctor (mole 'copy)
                                 ttype-iter))
               (force ttype)))
      ; We already have a constructor
      (work-with-ctor tpath mole val))))

; Will modify mole to expand a specific ctor.
; Returns: a pair containing the modified molecule
; and a list of new targets.
(def (work-with-ctor rpath mole ctor)
  (check-timer)

  ; We will be working under a relative path
  (def (padder p) (pad rpath p))
  (def targets null)

  ; call/cc here for early escape from inconsistency
  (call/cc
    (lam (k)
      (for-each
        (lam (ctor-iter)
          (match ctor-iter
            [(Form path constructor)
             (when (eq? 'INCONSISTENT
                        ((mole 'update) (padder path)
                                        constructor))
                   (k 'INCONSISTENT))]
            [(SLink paths)
             (for-each
               (lam (path-pair)
                 (when
                   (eq? 'INCONSISTENT
                        ((mole 'add-slink) (padder (car path-pair))
                                           (padder (cdr path-pair))))
                   (k 'INCONSISTENT)))
               (pair-iter paths))]
            [(Rec component type)
             ; If it's a link then it's not a target
             (unless ((mole 'link?) (padder component))
               (set! targets
                     (cons (Target (padder component) type)
                           targets)))]))
        ctor)
      (cons mole targets))))

(def (pair-iter ls)
  (foldr (lam (first rest)
           (if (null? rest)
               null
             (cons (cons first (car rest))
                   rest)))
         null
         ls))
