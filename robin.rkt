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



; `saved` is an engine, `stream` is a non-empty stream.
; `saved` will finish with `(stream-first stream)` as result.
(struct Pair (saved stream))

; The first item is always computed, since it's
; non-deterministic what the first item is.
(def (stream-robin streams)
  ; Kick-start the core
  (stream-robin-core
    (map (lam (stream)
           (Pair (proc->engine
                   (lam () (stream-first stream)))
                 stream))
         (remq empty-stream streams))))

(def (stream-robin-core pairs)
  (cond
    [(null? pairs) empty-stream]

    [else
     (let* ([focus  (car pairs)]
            [saved  (Pair-saved focus)]
            [stream (Pair-stream focus)]
            [others (cdr pairs)])
       (saved 1
         (lam (value ticks)
           (stream-cons
             value
             ; Completers get another go
             (if (stream-empty? (stream-rest stream))
                 (stream-robin-core others)
               (stream-robin-core
                 (cons (Pair (proc->engine (lam () (stream-ref stream 1)))
                             (stream-rest stream))
                       others)))))
         ; Failure: save progress and shove it to the end
         (lam (resume-eng)
           (stream-robin-core
             (append1 others
                      (Pair resume-eng stream))))))]))
