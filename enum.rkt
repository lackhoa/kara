#lang racket
(require "lang/kara.rkt"
         "engine.rkt"
         "robin.rkt"
         "mole.rkt"
         "types.rkt")
(provide (all-defined-out)
         (all-from-out "types.rkt")
         (all-from-out "mole.rkt"))

;; Returns: a single complete molecule, or 'NO-VALUE.
(def (general-search mole queue-fn)
  ;; We keep track of the molecules we've worked on
  ;; by tagging them with the "expanded" role.
  (def (expanded? m path)
    (neq? (send m ref (pad path 'expanded))
          'NOT-FOUND))

  ;; Returns: a stream of paths.
  (def (level-iter m [relative null])
    (stream-cons relative
                 (stream-interleave
                  (map (lam (role)
                         (level-iter (send m refr role)
                                     (pad relative role)))
                       ;; `remq*` weeds out the data.
                       (remq* '(expanded ctor type)
                              (send m get-roles))))))

  ;; WORK BEGINS: Find a molecule to work with.
  ;; Returns: a path | 'NO-MORE-TARGETS.
  (let loop ([nodes (list mole)])
    (match nodes
      [(list)  'NO-VALUE]

      [(cons mfocus mrest)
       (def next-target
         (let loop ([lvl-stream
                     (level-iter mfocus)])
           (def (recur)
             (loop (stream-rest lvl-stream)))

           (if (stream-empty? lvl-stream)
               'NO-MORE-TARGETS
               (let* ([pfocus
                       (stream-first lvl-stream)]
                      [mfocus
                       (send mfocus ref pfocus)])
                 (match* ((send mfocus get-ctor)
                          (send mfocus get-type))
                   ;; We know it's an entailment,
                   ;; not sure about the constructor.
                   [('?DATA (== entailment eq?))
                    pfocus]

                   ;; We know the constructor already.
                   [(_ (== entailment eq?))
                    (match (expanded? mfocus pfocus)
                      [#f pfocus]
                      [#t (recur)])]

                   ;; Not an entailment, not our job
                   [(_ _)
                    (recur)])))))

       (match next-target
         ;; Good news.
         ['NO-MORE-TARGETS  mfocus]

         [target
          (let ([new-moles
                 (expand mfocus target)])
            (for-each (lam (new-mole)
                        ;; Tag it
                        (send new-mole update-path
                          (pad target 'expanded)
                          #t))
                      new-moles)

            (loop (queue-fn mrest
                            new-moles)))])])))

(def (bfs mole)
  (general-search mole append))

(def (dfs mole)
  (general-search mole (flip append)))


;; Returns: a (possibly empty) list of consistent molecules,
;; whose constructors are finalized (and unique).
;; `target`: a path
(def (expand mole target)
  (match* ((send mole ref-ctor target)
           (send mole ref-type target))
    ;; Many constructors to choose from.
    [('?DATA (Union stream-ctors))
     (let ([result null])
       (stream-for-each
        (lam (ctor)
          (match ctor
            [(Ctor _ recs forms links)
             ;; Cloning is the biggest part
             (let* ([mclone
                     (send mole copy)])
               (send mclone update-path
                 (pad target 'ctor)
                 ctor)
               ;; Send it off to process-ctor
               (match (process-ctor (send mclone ref target)
                                    recs forms links)
                 ['CONFLICT (void)]
                 ['OK (cons! mclone result)]))]))
        stream-ctors)
       result)]

    ;; Already has a constructor, but haven't expanded it.
    [((Ctor _ recs forms links) _)
     (match (process-ctor (send mole ref target)
                          recs forms links)
       ['CONFLICT null]
       ['OK       (list mole)])]))

;; Returns: nothing, but mutate `mole`.
(def (process-ctor mole
                   recs forms links)
  (check-timer)  ;; This is a major time consumer

  (let/cc escape
    (for-each (lam (recs-iter)
                (match recs-iter
                  [(Rec role type)
                   (send mole update-path
                     (list role 'type)
                     type
                     (thunk (escape 'CONFLICT)))]))
              recs)

    (for-each (lam (forms-iter)
                (match forms-iter
                  [(Form path constructor)
                   (send mole update-path
                     (pad path 'ctor)
                     constructor
                     (thunk (escape 'CONFLICT)))]))
              forms)

    (for-each (lam (links-iter)
                (match links-iter
                  [(SLink p1 p2)
                   (send mole sync-path
                     p1
                     p2
                     (thunk (escape 'CONFLICT)))]))
              links)

    'OK))
