#lang racket
(require "lang/kara.rkt"
         "engine.rkt")

; When the engines return streams
; This function returns a stream, but it must compute the first item when you call it. I'll see if that needs to be fixed.
(def (fair-interleave streams)
  (let core
    ([splits
      (lmap (lam (remove null streams)  ; remove null since we use `car`
              (lcons (proc->engine
                       (const (car stream)))
                     (cdr stream))))])
    (if (null? splits)
        null
      (let* ([focus   (car splits)]
             [car-eng (car focus)]
             [rest    (cdr focus)]
             [others  (cdr splits)])
        ; Run the engine
        (car-eng 1
          ;Complete
          (lam (ticks value)
            (if (null? rest)
                (cons value
                      (round-robin (others)))
              (let ([nonce
                     (cons (proc->engine (const (car rest)))
                           (cdr rest))])
                (cons value
                      (core (cons nonce others))))))
          ; Failure
          (lam (resume-eng)
            (core (append others
                          (cons resume-eng rest)))))))))
