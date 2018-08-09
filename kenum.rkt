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
    (let* ([t-first (car targets)]
           [t-rest  (cdr targets)]
           [result (advance mole t-first)])
      (if (eq? result 'INCONSISTENT)
          null
        (multitask
          (for-each
            (lam (result-iter)
              (enqueue the new targets)
              (enum result-mole targets))
            result))))))

; Output: a list of molecules, or INCONSISTENT error flag
(def (advance mole target)
  (if (target has type)
      (map (lam (ctor)
             (work-with-ctor (mole 'copy) ctor))
           type-of-target)
    (work-with-ctor mole (mole-ref target-path))))

; Will modify mole to expand a specific ctor
(def (work-with-ctor mole ctor)
  (call/cc
    (lam (k)
      (for-each
        (lam (ctor-iter)
          (match ctor-iter
            [(form path constructor)
             (when (eq? 'INCONSISTENT
                        ((mole 'update) path constructor))
                   (k 'INCONSISTENT))]
            [(slink path1 path2)
             (when (eq? 'INCONSISTENT
                        ((mole 'add-slink) path1 path2))
                   (K 'INCONSISTENT))]
            [(rec name type)
             (unless (the path is in the slt)
               (add that target somewhere to return later))]))
        ctor)
      (Return the molecule and the targets))))
