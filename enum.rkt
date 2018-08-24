#lang racket
(require "lang/kara.rkt"
         "engine.rkt"
         "robin.rkt"
         "mole.rkt"
         "types.rkt")
(provide (all-defined-out)
         (all-from-out "types.rkt")
         (all-from-out "mole.rkt"))

; RNG for the enumeration
(def (take-random ls from-ten)
  (if (= from-ten 10)
      ls
    (let ([result null])
      (for-each
        (lam (item)
           (when (< (random 10)
                    from-ten)
             (cons! item result)))
        ls)
      result)))

; Iterative Deepening Search
(def (enum1 mole)
  (let loop ([cutoff 10])
    (match (enum-co mole cutoff)
      ; Got cut-off: double the cutoff limit
      ['CUT-OFF
       (loop (* 2 cutoff))]

      ; Impossible to find a value
      ['NO-VALUE
       'NO-VALUE]

      ; Got a value
      [result
       result])))

; Returns: a single complete molecule,
; or CUT-OFF, or NO-VALUE.
(def (enum-co mole cutoff)
  ; We keep track of the molecules we've worked on
  ; by tagging them with the "expanded" role.
  ; `p` is a path.
  (def (expanded? p)
    (match (send mole
             ref (pad p 'expanded))
      ['NOT-FOUND #f]
      [_          #t]))

  ; Returns: a stream of paths.
  (def (level-iter m [relative null])
    (stream-cons
      relative
      (stream-interleave
        (map (lam (role)
               (level-iter (send m refr role)
                           (pad relative role)))
             ; `remq*` weeds out the data.
             (remq* '(expanded ctor type)
                    (send m get-roles))))))

  (cond
    [(<= cutoff 0) 'CUT-OFF]

    [else
     ; WORK BEGINS: Find a molecule to work with.
     ; Returns: a path | 'NO-MORE-TARGETS.
     (def next-target
       (let loop ([lvl-stream
                   (level-iter mole)])
         (def (recur)
           (loop (stream-rest lvl-stream)))

         (if (stream-empty? lvl-stream)
             'NO-MORE-TARGETS
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

     (match next-target
       ; Good news.
       ['NO-MORE-TARGETS
        mole]

       [target
        (def cutoff-flag #f)

        (let/cc escape
          (for-each
            (lam (new-mole)
              ; Tag it so we don't expand it in the future.
              (send new-mole
                update-path (pad target
                                 'expanded)
                            #t)
              (match (enum-co new-mole
                              (- cutoff 1))
                ['NO-VALUE
                 (void)]

                ['CUT-OFF
                 (set! cutoff-flag
                       #t)]

                [result
                 (escape result)]))
            (expand mole target))

          (if cutoff-flag
              'CUT-OFF
            'NO-VALUE))])]))

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
                  update-path (pad target 'ctor)
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
             update-path (pad path 'ctor)
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
