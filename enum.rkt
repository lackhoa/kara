#lang racket
(require "lang/kara.rkt"
         "engine.rkt"
         "gen-robin.rkt"
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

; Returns: a generator of complete molecules
(def (enum mole targets)
  (generator ()
    (if (null? targets)
        (begin (yield mole)
               'DONE)
      (let* ([t-first (car targets)]
             [t-rest  (cdr targets)]
             [adv     (advance mole t-first)])
        ; Multitask all the different branches.
        (def multitasker
          (gen-robin
            (map
              (lam (adv-iter)
                (let ([new-mole    (car adv-iter)]
                      [new-targets (cdr adv-iter)])
                  ; Remember: `enum` returns a generator
                  (enum new-mole
                        (append t-rest new-targets))))
              adv)))
        ; Now, multitasker is a generator, the job now
        ; is just return any value that it returns.
        (let loop ()
          (yield (multitasker))
          (loop))))))

; Output: a (possibly empty) list of
; consistent molecule-targets pairs.
(def (advance mole target)
  (def tpath
    (Target-path target))
  (def ttype
    (Target-type target))

  (def (explore-type)
    (match ttype
      [(Union ctors)
       (remq* '(CONFLICT)
         (map (lam (ctor)
                (match ctor
                  [(Ctor _ recs forms links)
                   (let* ([mclone (send mole copy)])
                     (send mclone
                       update-path tpath ctor)
                     (process-ctor tpath
                                   mclone
                                   recs
                                   forms
                                   links))]))
              (force ctors)))]
      ; If the type is unenumerable then we just ignore it.
      ['ANY (send mole update-path tpath 'UNKNOWN)]))

  (match (send mole ref tpath)
    [#f (explore-type)]

    [(? (curryr is-a? mole%) m)
     (match (send m get-data)
       ['UNKNOWN (explore-type)]

       ; We already have chosen the constructor.
       [(Ctor _ recs forms links)
        (match (process-ctor tpath
                             mole
                             recs
                             forms
                             links)
          ['CONFLICT null]
          [result (list result)])])]))

; Returns: a pair containing the modified molecule
; and a list of new targets.
(def (process-ctor rpath
                   mole
                   recs
                   forms
                   links)
  (check-timer)  ; This is a major time waster

  ; We will be working under a relative path
  (def (pad p) (append rpath p))
  (def new-targets null)

  (let/cc escape
    (for-each
      (lam (recs-iter)
        (match recs-iter
          [(Rec role type)
           (cons! (Target (pad (list role))
                          type)
                  new-targets)]))
      recs)

    (for-each
      (lam (forms-iter)
        (match forms-iter
          [(Form path constructor)
           (send mole
             update-path (pad path)
                         constructor
                         (lam () (escape 'CONFLICT)))]))
      forms)

    (for-each
      (lam (links-iter)
        (match links-iter
          [(SLink p1 p2)
           (send mole
             sync-path (pad p1)
                       (pad p2)
                       (lam () (escape 'CONFLICT)))]))
      links)
    (cons mole new-targets)))
