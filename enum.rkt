#lang racket
(require "lang/kara.rkt"
         "engine.rkt"
         "robin.rkt"
         "mole.rkt"
         "types.rkt")
(provide (all-defined-out)
         (all-from-out "types.rkt")
         (all-from-out "mole.rkt"))

;; Returns: a stream of paths.
(def (level-iter m [relative null])
  (stream-cons relative
               (stream-interleave
                (for ([role (send m get-roles)])
                  (level-iter (send m refr role)
                              (pad relative role))))))

;; Returns: a single complete molecule, or 'NO-VALUE.
(def (general-search mole
                     queue-fn
                     [focused-types (list entailment)])
  (let loop ([nodes (list mole)])
    (match nodes
      [(list)  'NO-VALUE]

      [(cons mfocus mrest)
       (def next-target
         ;; Returns: a path | 'NO-MORE-TARGETS.
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

                 (cond [(memq (send mfocus get-type)
                              focused-types)
                        (match (send mfocus get-ctor)
                          ['?DATA  pfocus]

                          [_  (match (send mfocus get-expanded?)
                                [#f  pfocus]
                                [#t  (recur)])])]

                       [else  (recur)])))))

       (match next-target
         ;; Good news.
         ['NO-MORE-TARGETS  mfocus]

         [target
          (let ([new-moles
                 (expand mfocus target)])
            (for ([new-mole new-moles])
              ;; Tag it so we don't expand again
              (send new-mole mark-expanded))

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
  (match (send mole ref-ctor target)
    ['?DATA
     ;; Many constructors to choose from.
     (let ([result null])
       (for ([ctor (send mole ref-type target)])
         (match ctor
           [(Ctor _ recs forms links)
            (let* ([mclone
                    (send mole copy)])
              ;; Cloning is the biggest part
              (send mclone update-path
                (pad target 'ctor)
                ctor)

              (match (process-ctor! (send mclone ref target)
                                    recs forms links)
                ;; Send it off to process-ctor!
                ['CONFLICT  (void)]
                [_          (cons! mclone result)]))]))
       result)]

    [(Ctor _ recs forms links)
     ;; Already has a constructor, but haven't expanded it.
     (match (process-ctor! (send mole ref target)
                           recs forms links)
       ['CONFLICT null]
       [_         (list mole)])]))

;; Returns: nothing, but mutate `mole`.
(def (process-ctor! mole
                    recs forms links)
  (check-timer)  ;; This is a major time consumer

  (let/cc escape
    (for ([ri recs]
          [i  (in-naturals)])
      (send mole set-type-path
        [list i] ri))

    (for ([fi forms])
      (match fi
        [(cons path constructor)
         (send mole update-path
           path
           constructor
           (thunk (escape 'CONFLICT)))]))

    (for ([li links])
      (send mole sync-paths
        li
        (thunk (escape 'CONFLICT))))))
