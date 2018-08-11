#lang racket
(require "lang/kara.rkt")

(struct Target path [type #:auto]
  #:auto-value 'UNSPECIFIED)

; Either kick-start or state the type of the target
(def (kick-start mole)
  (let* ([targets
          (enqueue! (new-queue) (Target ""))])
    (enum mole targets)))

(def (enum mole targets)
  (if (null? targets)
      mole
    (let* ([t-first (target-first targets)]
           [t-rest  (target-rest  targets)]
           [result  (advance mole t-first)])
      (if (eq? result 'INCONSISTENT)
          null
        (for-each
          (lam (result-iter)
            (add-targets (t-rest))
            (enum result-mole targets))
          result)))))

; Output: a list of molecules, or INCONSISTENT error flags
(def (advance mole target)
  (let* ([tpath (target-path target)]
         [ttype (target-type target)]
         [val   ((mole 'ref) tpath)])
    (if (eq? val 'NOT-FOUND)
        (remove 'INCONSISTENT
          (map (lam (ttype-iter)
                 (work-with-ctor (mole 'copy)
                                 ttype-iter))
               (force ttype)))
      ; We already have a constructor
      (work-with-ctor tpath mole val))))

; Will modify mole to expand a specific ctor
(def (work-with-ctor rpath mole ctor)
  ; We will be working under a relative path
  (def (padder p)
    (pad rpath p))

  (def targets null)

  (call/cc
    (lam (k)
      (for-each
        (lam (ctor-iter)
          (match ctor-iter
            [(Form path constructor)
             (when (eq? 'INCONSISTENT
                        ((mole 'update) (padder path) constructor))
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
             (unless ((mole 'link?) (padder component))
               (set! targets
                     (cons (Target (padder component) type)
                           targets)))]))
        ctor)
      (cons mole targets))))

(def (pair-iter ls)
  (foldr (lam (x y)
           (if (null? y)
               null
             (cons (cons x (car y))
                   y)))
         null
         ls))
