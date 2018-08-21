#lang racket
(require "lang/kara.rkt"
         "engine.rkt"
         "gen-robin.rkt"
         "mole.rkt"
         "types.rkt")
(provide (all-defined-out)
         (all-from-out "types.rkt")
         (all-from-out "mole.rkt"))

; Returns: a generator of complete molecules
(def (enum mole targets)
  ; We keep track of the molecules we've worked on
  ; by tagging them with the "expandd" role.
  ; `p` is a path.
  (def (expandd? p)
    (send mole
      ref (append1 p 'expandd)))

  ; Returns: a stream of paths.
  (def (level-iter m [relative null])
    (stream-cons
      relative
      (stream-interleave (map (lam (role)
                                (level-iter (send m ref role)
                                            (append1 relative role)))
                              (send m get-roles)))))

  ; Find a molecule to work with.
  ; Returns: a molecule, or #f if m is complete.
  (def find-target
    (let loop ([lvl-stream (level-iter mole)])
      (def (recur)
        (loop (stream-rest lvl-stream)))

      (if (stream-empty? lvl-stream)
          'NO-MORE-TARGETS
        ; Notice that the every molecule has a constructor.
        (let ([focus (stream-first lvl-stream)])
          (match (send mole
                   ref-data (append1 focus 'ctor))
            [(Union _) focus]
            [(Ctor _ _ _ _)
             (cond [(expandd focus) (recur)]
                   [else focus])]
            [(or 'UNKNOWN 'ANY) (recur)])))))

  (generator ()
    (match find-target
      ['NO-MORE-TARGETS
       (yield mole) 'DONE]

      [target
       ; Multitask all the different branches.
       (gen-impersonate
         (gen-robin
           (map
             (lam (new-mole)
               ; Tag it.
               (send new-mole
                 update-role 'expandd #t)
               ; Remember: `enum` returns a generator
               (enum new-mole))
             (expand mole target))))])))

(def (pad relative role)
  (append1 relative role))

; Returns: a (possibly empty) list of consistent molecules.
; `target`: a path
(def (expand mole target)
  (match (send mole
           ref-data (append1 target 'ctor))
    ; Many constructors to choose from.
    [(Union ctors)
     (let ([result null])
       (for-each
         (lam (ctor)
           (match ctor
             [(Ctor _ recs forms links)
              ; Cloning is the biggest part
              (let* ([mclone (send mole copy)])
                (send mclone
                  mutate-path (append1 target 'ctor) ctor)
                (for-each
                  (lam (code)
                    (match code
                      ['CONFLICT (void)]
                      ['OK (cons! mclone result)]))
                  (process-ctor (send mclone ref target)
                                recs forms links))
                mclone)]))
         (force ctors))
         result)]
    ; Already has a constructor, but haven't expanded it.
    [(Ctor _ recs forms links)
     (match (process-ctor (send mole ref target)
                          recs forms links)
       ['CONFLICT null]
       ['OK       (list mole)])]))

; Returns: nothing, but mutate `mole`.
(def (process-ctor mole
                   recs forms links)
  (check-timer)  ; This is a major time consumer

  (let/cc escape
    (for-each
      (lam (recs-iter)
        (match recs-iter
          [(Rec role type)
           (send mole
             update-path '(role) type)]))
      recs)

    (for-each
      (lam (forms-iter)
        (match forms-iter
          [(Form path constructor)
           (send mole
             update-path path
                         constructor
                         (lam () (escape 'CONFLICT)))]))
      forms)

    (for-each
      (lam (links-iter)
        (match links-iter
          [(SLink p1 p2)
           (send mole
             sync-path p1
                       p2
                       (lam () (escape 'CONFLICT)))]))
      links)))
