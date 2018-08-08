#lang racket
(require "lang/kara.rkt")

; Output: a list of expanded molecules, or INCONSISTENT error flag
(def (expand mole)
  (if (null? thunks)
      'STABLE)
    (let* ([thunks (mole 'thunks)]
           [ft (car thunks)]
           [path (thunk-path ft)]
           [type (thunk-type ft)])
      (if (path-in-molecule-containing-the-form)
          (let ([ctor (mole-ref path)]
            (if ctor-not-in-type
                (error "ENUM" "Invalid constructor???" ctor)
              (list (work-with-ctor mole ctor)))
        ; Consider multiple possibilities
        (map (lam (ctor)
               (work-with-ctor (mole 'copy) ctor))
             type))))))

; Will modify mole to expand a specific ctor
(def (work-with-ctor mole ctor)
  (call/cc (k)
    (for-each
      (lam (ctor-iter)
        (match ctor-iter
          [(form path constructor)
           (when (eq? 'INCONSISTENT
                      ((mole 'update) path constructor))  ; Returns false if the form is wrong
                 (k 'INCONSISTENT))]
          [(slink path1 path2)
           (when (eq? 'INCONSISTENT
                      ((mole 'add-slink) path1 path2))
                 (K 'INCONSISTENT))]
          [(rec name type)
           add-more-thunks]))
      ctor)))
