#lang racket
(require racket/list
         "lang/kara.rkt"
         "engine.rkt"
         "robin.rkt"
         "mole.rkt"
         "types.rkt")
(provide (all-defined-out)
         (all-from-out "types.rkt")
         (all-from-out "mole.rkt"))

(def (level-iter m [relative null])
  ;; Returns: a stream of paths.
  (stream-cons relative
               (stream-interleave
                (for/list ([role (send m get-roles)])
                  (level-iter (send m refr role)
                              (pad relative role))))))

(def (get-target mole focused-types)
  ;; Returns: a path | 'NO-MORE-TARGETS.
  (let loop ([lvl-stream
              (level-iter mole)])
    (cond [(stream-empty? lvl-stream)  'NO-MORE-TARGETS]
          [else
           (let* ([pfocus  (stream-first lvl-stream)]
                  [mfocus  (ref mole pfocus)]
                  [prest   (stream-rest lvl-stream)])
             (cond [(memq (send mfocus get-type)
                          focused-types)
                    (match (send mfocus get-data)
                      ['?DATA  pfocus]
                      [_       (match (send mfocus get-expanded?)
                                 [#f  pfocus]
                                 [#t  (loop prest)])])]
                   [else  (loop prest)]))])))

(def (general-prove mole
                    queue-fn
                    [focused-types (list entailment)])
  ;; Returns: a single complete molecule, or 'NO-VALUE.
  (let/ec return
    (let loop ([nodes  (list (cons mole null))])
      (match nodes
        [(list)  'NO-VALUE]

        [(cons (cons mfocus target)
               rest)
         (let ([new-moles  (expand mfocus target)]
               [new-nodes  null])
           (for ([new-mole new-moles])
             (match (get-target new-mole
                                focused-types)
               ['NO-MORE-TARGETS  (return new-mole)]  ; Good news.
               [new-target
                (send (ref new-mole target) mark-expanded) ; Tag it so we don't expand again
                (cons! (cons new-mole new-target)
                       new-nodes)]))

           (loop (queue-fn rest
                           new-nodes)))]))))

(def (bfs mole)
  (general-prove mole append))

(def (dfs mole)
  (general-prove mole (flip append)))

(def (general-enum mole
                   queue-fn
                   [focused-types (list entailment)])
  ;; Returns: a stream of complete molecules
  (generator ()
    (let loop ([nodes  (list (cons mole null))])
      (match nodes
        [(list)  'DONE]

        [(cons (cons mfocus target)
               rest)
         (let ([new-moles  (expand mfocus target)]
               [new-nodes  null])
           (for ([new-mole new-moles])
             (match (get-target new-mole
                                focused-types)
               ['NO-MORE-TARGETS  (yield new-mole)]  ; Good news.
               [new-target
                (send (ref new-mole target) mark-expanded) ; Tag it so we don't expand again
                (cons! (cons new-mole new-target)
                       new-nodes)]))

           (loop (queue-fn rest
                           new-nodes)))]))))


(def (bfs-enum mole)
  (general-enum mole append))

(def (expand mole target)
  ;; Returns: a (possibly empty) list of consistent molecules,
  ;; whose constructors are finalized (and unique).
  ;; `target`: a path
  (match (ref-data mole target)
    ['?DATA
     ;; Many constructors to choose from.
     (let ([result null]
           [type   (ref-type mole target)])
       (check-false (eq? type '?TYPE))

       (for ([ctor type])
         (match ctor
           [(Ctor _ recs forms links)
            (let* ([mclone
                    (send mole copy)])
              ;; Cloning is the biggest part
              (update-path mclone target ctor)

              (match (process-ctor! (ref mclone target)
                                    recs forms links)
                ;; Send it off to process-ctor!
                ['CONFLICT  (void)]
                [_          (cons! mclone result)]))]))
       result)]

    [(Ctor _ recs forms links)
     ;; Already has a constructor, but haven't expanded it.
     (match (process-ctor! (ref mole target)
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
      (set-type-path mole [list i] ri))

    (for ([fi forms])
      (match fi
        [(cons path constructor)
         (update-path mole path constructor
                      (thunk (escape 'CONFLICT)))]))

    (for ([li links])
      (sync-paths mole li
                  (thunk (escape 'CONFLICT))))))

(def (ccs mole)
  ;; Function to get the conclusion
  (send mole refr 0))

(def (cleanup moles)
  (let ([sorted
         ;; Sort according to complexity of the conclusion
         (sort moles
               (lam (small large)
                 (< (complexity (ccs small))
                    (complexity (ccs large)))))])
    (match sorted
      [(list)  (list)]
      [(cons start focus)
       (let loop ([primes (list start)]
                  [focus   focus])
         (match focus
           [(list)  primes]
           [(cons fst rest)
            (let ([ccs-replaceable?
                   (lam (m1 m2) (replaceable? (ccs m1)
                                            (ccs m2)))])
              (match (exists (curry ccs-replaceable? fst)
                             primes)
                [#f       (loop (append1 primes fst)
                                rest)]
                [witness  (displayln (cons (ccs witness)
                                           (ccs fst)))
                          (loop primes rest)]))]))])))

(def (unify? m1 m2)
  (match (send (send m1 copy) sync
           (send m2 copy) (thunk 'FAIL))
    ['FAIL  #f]
    [_      #t]))



(define (replaceable? m mr)
  ;; mr can replace m
  ;; (provided, both are complete regarding the focused types)
  (def (hardened mole)
    (def (mole->list mole)
      (flatten (cons mole
                     (map mole->list
                          (send mole get-kids)))))

    (let* ([clone (send mole copy)]
           [all   (mole->list clone)])
      (for ([m all])
        (send m mark-no-touch)
        (send m set-no-sync
          ;; Disallow any additional internal synchronization.
          (set-subtract all (send m get-sync-ls))))

      clone))

  (let ([m-hard   (hardened m)]
        [mr-clone (send mr copy)])
    (send m-hard sync
      mr-clone (thunk #f))))
