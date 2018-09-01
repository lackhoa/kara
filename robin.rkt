#lang racket
(require "lang/kara.rkt"
         "engine.rkt")
(provide gen-robin
         stream-robin)

(def (gen-robin gens)
  (generator ()
    (let loop ([engs (map proc->engine gens)])
      (if (null? engs)
          'DONE
        (let* ([focus  (car engs)]
               [saved  null]
               [gen    null]
               [others (cdr engs)])
          (if (pair? focus)
              ; There is saved progress in the first item
              (begin (set! saved (car focus))
                     (set! gen   (cdr focus)))
            ; If there's no saved progress then ignore it
            (begin (set! saved focus)
                   (set! gen   focus)))
            (saved 1
              (lam (value ticks)
                (if (eq? 'DONE value)
                    (loop others)
                  (begin
                    (yield value)
                    ; Completers get another go
                    (loop (cons gen others)))))
              ; Failure: save progress and shove it to the end
              (lam (resume-eng)
                (loop (append others
                              (list (cons resume-eng
                                          gen)))))))))))




(def (stream-robin streams)
  ; Kick-start the core
  (stro-core
    (map (lam (stream)
           (Pair (proc->engine
                   (thunk (stream-first stream)))
                 stream))
         (remq empty-stream streams))
    'NOT-YET-COMPUTED))

; `saved` is an engine, `stream` is a non-empty stream.
; `saved` will finish with `(stream-first stream)` as result.
(struct Pair (saved stream))

(def _stream-empty? stream-empty?)
(def _stream-first  stream-first)
(def _stream-rest   stream-rest)

(struct stro-core ([pairs #:mutable]
                   [next  #:mutable])
  #:methods gen:stream
  [(define (stream-empty? stro)
     (null? (stro-core-pairs stro)))

   (define (stream-first stro)
     (let* ([pairs   (stro-core-pairs stro)]
            [focus   (car pairs)]
            [others  (cdr pairs)]
            [saved   (Pair-saved focus)]
            [pstream (Pair-stream focus)])
       (saved 1
         (lam (value ticks)
           ; Set the next values
           (if (_stream-empty? (_stream-rest pstream))
               (set-stro-core-next! stro
                 (stro-core others
                            'NOT-YET-COMPUTED))
             ; Completers get another go
             (set-stro-core-next! stro
               (stro-core
                 (cons (Pair (proc->engine
                               (thunk (stream-ref pstream 1)))
                             (_stream-rest pstream))
                       others)
                 'NOT-YET-COMPUTED)))


           ; And don't forget to return the result
           value)

         ; Failure: save progress, shove it to the end, then try again.
         (lam (resume-eng)
           (set-stro-core-pairs! stro
             (append1 others
                      (Pair resume-eng pstream)))
           (stream-first stro)))))

   (define (stream-rest stro)
     (match (stro-core-next stro)
       ['NOT-YET-COMPUTED
        (stream-first stro)
        (stro-core-next stro)]

       [any any]))])
