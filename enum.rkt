#lang racket
(require "lang/kara.rkt"
         "engine.rkt"
         "gen_robin.rkt"
         "mole.rkt"
         "types.rkt")
(provide (all-defined-out)
         (all-from-out "types.rkt")
         (all-from-out "mole.rkt"))

(struct Target (path type))

; start enumerating with the root as the target
(def (kick-start mole [root-type 'NOT-NEEDED])
  (enum mole
        (list (Target null root-type))))

(def (get-one mole [root-type 'NOT-NEEDED])
  ((kick-start mole root-type)))

; Returns: a generator of complete molecules
(def (enum mole targets)
  (if (null? targets)
      (generator ()
        (yield mole)
        'DONE)
    (let* ([t-first (car targets)]
           [t-rest  (cdr targets)]
           [adv     (advance mole t-first)])
      ; Multitask all the different branches.
      (gen-robin
        (map
          (lam (adv-iter)
            (let ([new-mole    (car adv-iter)]
                  [new-targets (cdr adv-iter)])
              ; Remember: `enum` returns a generator
              (enum new-mole
                    (append t-rest new-targets))))
          adv)))))
(trace enum)

; Output: a (possibly empty) list of
; consistent molecule-targets pairs.
(def (advance mole target)
  (def tpath
    (Target-path target))
  (def ttype
    (Target-type target))

  (def (explore-type)
    (remq* '(CONFLICT)
            (map (lam (ctor)
                  (match ctor
                    [(Ctor name body)
                     (process-ctor tpath
                                   (send mole copy)
                                   body)]))
              (match ttype
                [(Type body) (force body)]))))

  (match (send mole ref tpath)
    [#f (explore-type)]

    [(? (curryr is-a? mole%) m)
     (match (send m get-data)
       ['UNKNOWN (explore-type)]

       ; We already have chosen tthe constructor.
       [(Ctor name body)
        (match (process-ctor tpath
                             mole
                             body)
          ['CONFLICT null]
          [result (list result)])])]))

; Returns: a pair containing the modified molecule
; and a list of new targets.
(def (process-ctor rpath mole ctor-body)
  (check-timer)  ; This is a major time waster

  ; We will be working under a relative path
  (def (pad p) (append rpath p))
  (def new-targets null)

  (let/cc escape
    (for-each
      (lam (ctor-iter)
        (match ctor-iter
          [(Form path constructor)
           (send mole
             update-path (pad path)
                         constructor
                         (lam () (escape 'CONFLICT)))]

          [(SLink p1 p2)
           (send mole
             sync-path (pad p1)
                       (pad p2)
                       (lam () (escape 'CONFLICT)))]

          [(Rec role type)
           (cons! (Target (pad (list role))
                          type)
                  new-targets)]))
      ctor-body)
    (cons mole new-targets)))
