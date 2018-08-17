#lang racket
(require "lang/kara.rkt"
         "engine.rkt"
         "gen_robin.rkt"
         "mole.rkt"
         "types.rkt")

(struct Target (path type))

; start enumerating with the root as the target
(def (kick-start mole [root-type 'NOT-NEEDED])
  (enum mole
        (QTars (list (Target root-type)))))

; Returns: a generator of complete molecules
(def (enum mole targets)
  (generator ()
    (if (null? targets)
        (begin (yield mole)
               (yield 'DONE))
      (let* ([t-first (cons targets)]
             [t-rest  (cdr targets)]
             [adv     (advance mole t-first)])
        ; Multitask all the different branches
        (gen-robin
          (map (lam (adv-iter)
                 (let ([new-mole    (car adv-iter)]
                       [new-targets (cdr adv-iter)])
                   ; `enum` returns a generator
                   (enum new-mole
                         (append t-rest new-targets))))
               adv))))))

; Output: a list of molecule-targets pairs.
(def (advance mole target)
  (let* ([tpath (Target-path target)]
         [ttype (Target-type target)]
         [lu    (send mole ref tpath)])
    (if (eq? lu 'NOT-FOUND)
        (remove 'CONFLICT
          (map (lam (ctor)
                 (process-ctor (send mole copy)
                                 ctor))
               (force ttype)))
      ; We already have a constructor
      (list (process-ctor tpath mole val)))))

; Will modify mole to expand a specific ctor.
; Returns: a pair containing the modified molecule
; and a list of new targets.
(def (process-ctor rpath mole ctor)
  (check-timer)

  ; We will be working under a relative path
  (def (pad p) (append rpath p))
  (def targets null)

  (let/cc escape
    (for-each
      (lam (ctor-iter)
        (match ctor-iter
          [(Form path constructor)
           (send mole
             update-path (pad path)
                         constructor
                         (lam () (escape 'CONFLICT)))]

          ; This part assumes that the molecules are present.
          [(SLink p1 p2)
           (send (send mole ref (pad p1))
             sync (send mole ref (pad p2))
                  (lam () (escape 'CONFLICT)))]

          [(Rec component type)
           (set! targets
             (cons (Target (padder component) type)
                   targets))]

          [else
           (error "process-ctor"
                  "Invalid constructor term"
                  ctor-iter)]))
      (Ctor-body ctor))
    (cons mole targets)))
