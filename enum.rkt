#lang racket
(require "lang/kara.rkt"
         "engine.rkt"
         "robin.rkt"
         "mole.rkt"
         "types.rkt")
(provide (all-defined-out)
         (all-from-out "types.rkt")
         (all-from-out "mole.rkt"))

; Returns: a stream of complete molecules
(def (enum mole)
  ; We keep track of the molecules we've worked on
  ; by tagging them with the "expanded" role.
  ; `p` is a path.
  (def (expanded? p)
    (match (send mole
             ref (append1 p 'expanded))
      ['NOT-FOUND #f]
      [_          #t]))

  ; Returns: a stream of paths.
  (def (level-iter m [relative null])
    (stream-cons
      relative
      (stream-interleave
        (map (lam (role)
               (level-iter (send m refr role)
                           (append1 relative role)))
             (remq* '(expanded ctor type)
                    (send m get-roles))))))

  ; Find a molecule to work with.
  ; Returns: a path, 'NO-MORE-TARGETS.
  (def next-target
    (let loop ([lvl-stream
                (level-iter mole)])
      (def (recur)
        (loop (stream-rest lvl-stream)))

      (if (stream-empty? lvl-stream)
          'NO-MORE-TARGETS
        ; Notice that the every molecule has a constructor.
        ; We know that we're only working on structures.
        (let* ([pfocus
                (stream-first lvl-stream)]
               [mfocus
                (send mole ref pfocus)])
          (match* ((send mfocus get-ctor)
                   (send mfocus get-type))
            ; No idea what this is.
            [('?DATA '?DATA) (recur)]

            ; We know the type, not the constructor.
            [('?DATA _) pfocus]

            ; We know the constructor already.
            [(_ _)
             (match (expanded? pfocus)
               [#f pfocus]
               [#t (recur)])])))))

  (generator ()
    (match next-target
      ['NO-MORE-TARGETS
       (yield mole) 'DONE]

      [target
       ; Multitask all the different branches.
       (gen-impersonate
         (gen-robin
           (map
             (lam (new-mole)
               ; Tag it so we don't expand it in the future.
               (send new-mole
                 update-path (append1 target
                                      'expanded)
                             #t)
               ; Remember: `enum` returns a stream
               (enum new-mole))
             (expand mole target))))])))


(def (pad relative role)
  (append1 relative role))

; Returns: a (possibly empty) list of consistent molecules,
; whose constructors are finalized (and unique).
; `target`: a path
(def (expand mole target)
  (match* ((send mole ref-ctor target)
           (send mole ref-type target))
    ; Many constructors to choose from.
    [('?DATA (Union stream-ctors))
     (let ([result null])
       (stream-for-each
         (lam (ctor)
           (match ctor
             [(Ctor _ recs forms links)
              ; Cloning is the biggest part
              (let* ([mclone
                      (send mole copy)])
                (send mclone
                  update-path (append1 target 'ctor)
                              ctor)
                  ; Send it off to process-ctor
                  (match (process-ctor (send mclone ref target)
                                       recs forms links)
                    ['CONFLICT (void)]
                    ['OK (cons! mclone result)]))]))
         stream-ctors)
         result)]

    ; Already has a constructor, but haven't expanded it.
    [((Ctor _ recs forms links) _)
     (match (process-ctor (send mole ref target)
                          recs forms links)
       ['CONFLICT null]
       ['OK       (list mole)])]))

; Returns: nothing, but mutate `mole`.
(def (process-ctor mole
                   recs forms links)
  (check-timer)  ; This is a major time consumer

  (let/cc escape
    (def get-me-out
      (lam () (escape 'CONFLICT)))

    (for-each
      (lam (recs-iter)
        (match recs-iter
          [(Rec role type)
           (send mole
             update-path (list role 'type)
                         type
                         get-me-out)]))
      recs)

    (for-each
      (lam (forms-iter)
        (match forms-iter
          [(Form path constructor)
           (send mole
             update-path (append1 path 'ctor)
                         constructor
                         get-me-out)]))
      forms)

    (for-each
      (lam (links-iter)
        (match links-iter
          [(SLink p1 p2)
           (send mole
             sync-path p1
                       p2
                       get-me-out)]))
      links)

    ; If we get here, then everything is fine.
    'OK))
