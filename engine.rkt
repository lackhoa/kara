#lang racket
(require "lang/kara.rkt"
         "timer.rkt")
(provide make-engine tlam (all-from-out "timer.rkt"))

; Reminder: an Engine is a thing that takes some fuel,
; an expire handling routine, and a completion handling routine.
(def (make-engine proc)
  ; `do-complete` takes ticks remaining and value.
  ; `do-expire` works with continuation.
  (let ([do-complete #f]
        [do-expire #f])
    ; I wonder what the point of the second argument is,
    ; when I replace it with some garbage, things still work fine.
    ; Also, I don't think `(call/cc do-expire)` returns ticks, it's just
    ; a weird way of calling function, since `start-timer` is fixed on
    ; its arity.
    (def (engine-expire-handler)
      (start-timer (call/cc do-expire) engine-expire-handler))

    (def (new-engine resume)
      ; `user-complete-handler` works with ticks and a value
      ; returned when a procedure completes.
      ; `user-expire-handler` works with an engine.
      (lam (ticks user-complete-handler user-expire-handler)
        ; Each application of `escape` returns here, or rather,
        ; the body of this list.
        ((call/cc
           ; This function sets up do-complete, do-expire
           ; and run the procedure `resume`.
           ; `escape` is the continuation here.
           (lam (escape)
             (set! do-complete
               (lam (ticks-left value)
                 ; Note that the code run by `user-complete-handler`
                 ; is delayed, so that the continuation will be passed
                 ; immediately to the one captured in `escape`.
                 (escape (lam ()
                           (user-complete-handler ticks-left value)))))

             (set! do-expire
               (lam (resume)
                 (escape (lam ()
                           (user-expire-handler (new-engine resume))))))

             (resume ticks))))))

    (new-engine
      (lam (ticks)
        ; Start the timer <could have done it before
        ; the code is invoked <this part>>
        (start-timer ticks engine-expire-handler)
        ; Do the computing.
        (let ([value (proc)])
          ; This code will run only after `proc` returned.
          (let ([ticks-left (stop-timer)])
            (do-complete ticks-left value)))))))

; timed-lambda: The clock will be checked every time the
; procedure is invoked.
(define-syntax-rule (tlam formals exp1 exp2 ...)
  (lam formals (check-timer) exp1 exp2 ...))
