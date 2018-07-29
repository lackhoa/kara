#lang racket
(require "lang/kara.rkt"
         "timer.rkt")
(provide make-engine tlam (all-from-out "timer.rkt"))

(def (new-engine resume)
  (lam (ticks complete expire)
    ((call/cc
       (lam (escape)
         (run resume
              (stop-timer)
              ticks
              (lam (value ticks-left)
                (escape (lam () (complete value ticks-left))))
              (lam (new-engine)
                (escape (lam () (expire new-engine))))))))))

(def (run resume parent-ticks current-ticks complete expire)
  (let ([ticks
         (if (and (active?) (< parent-ticks current-ticks))
             parent-ticks
             current-ticks)])
    ; One of the residual tick values will be 0.
    (push (- parent-ticks ticks) (- current-ticks ticks) complete expire)
    (resume ticks)))

; This function is in sole charge of starting the clock.
(def (go ticks)
  (when (active?)
    (if (= ticks 0)
        (timer-handler)
        (start-timer ticks timer-handler))))

(def (do-complete value ticks-left)
  (pop (lam (parent-ticks current-ticks complete expire)
         ; The parent must still time the completion process of the child.
         (go (+ parent-ticks ticks-left))
         (complete value (+ current-ticks ticks-left)))))

(def (do-expire resume)
  (pop (lam (parent-ticks current-ticks complete expire)
         (if (> current-ticks 0)
             ; This process still has time, keep looking among its ancestors.
             (do-expire (lam (ticks)
                          (run resume ticks current-ticks complete expire)))
             ; Found the process that ran out of time.
             (begin (go parent-ticks)
                    (expire (new-engine resume)))))))


; This part will be an eternal mystery,
; but I can't dismiss the `go` part.
(def (mysterious-call) (call/cc do-expire))
(def (timer-handler)
  (go (mysterious-call)))

; This is the interface.
(def (make-engine proc)
  (new-engine
    (lam (ticks)
      ; Note that the engine starts the clock
      (go ticks)
      (let ([value (proc)])
        (let ([ticks-left (stop-timer)])
          (do-complete value ticks-left))))))




; --------------------------------------
; Concerning the stack
; --------------------------------------
(def stack null)

(def (push . items)
  (set! stack (cons items stack)))

(def (pop handler)
  (if (null? stack)
      (error 'engine "Attempt to return from inactive engine")
      (let ([top (car stack)])
        (set! stack (cdr stack))
        (apply handler top))))

(def (active?) (not (null? stack)))





; timed-lambda: The clock will be checked every time the
; procedure is invoked.
(define-syntax-rule (tlam formals exp1 exp2 ...)
  (lam formals (check-timer) exp1 exp2 ...))

(trace mysterious-call)
(trace go)
