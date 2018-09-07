#lang racket
(require "lang/kara.rkt"
         "timer.rkt")
(provide proc->engine tlam (all-from-out "timer.rkt"))

;; The clock always gets started by `go` RIGHT before some work gets done.
;; `resume` represents any procedure that will run in the amount of ticks
;; passed to it (and it will always start the clock when it begins).
;; Due to the complication from nesting engines, `resume` is
;; always preceded with a call to `run`.
(def (resume->engine resume)
  (lam (new-ticks complete expire)
    ;; Why use `call/cc` here: if we didn't, `run` will still return
    ;; the completion of `resume`. However, if the timer expires, control
    ;; will never be able to return to this procedure.
    ;; Moral of the story: The timer handler is just another procedure, and
    ;; there is no "early return" in Scheme.
    ((call/cc
       (lam (escape)
         ;; Why stop the timer here: it will be restarted by `go` later.
         (let ([parent-ticks (stop-timer)])
           (run resume
                parent-ticks
                new-ticks
                (lam (value ticks-left)
                  (escape (thunk (complete value ticks-left))))
                (lam (resume-engine)
                  (escape (thunk (expire resume-engine)))))))))))

;; This procedure intelligently handles the clock (and the stack)
;; to works out the differences between parent and child
;; before `resume` starts the clock and do work.
(def (run resume parent-ticks current-ticks complete expire)
  (let ([ticks (if (and (active?)
                        (< parent-ticks current-ticks))
                   parent-ticks
                   current-ticks)])
    ;; One of the residual tick values will be 0,
    ;; and inactive parents will have negative ticks, but that won't matter.
    (push (- parent-ticks ticks) (- current-ticks ticks) complete expire)
    (resume ticks)))

;; This function is in sole charge of starting the clock.
(def (go ticks)
  (when (active?)
    (if (= ticks 0)
        (timer-handler)
        (start-timer ticks timer-handler))))

(def (do-complete value ticks-left)
  (pop (lam (parent-ticks current-ticks complete _expire)
         ;; The parent must still time the completion process of the child.
         (go (+ parent-ticks ticks-left))
         (complete value (+ current-ticks ticks-left)))))

(def (do-expire resume)
  (pop
    (lam (parent-ticks current-ticks complete expire)
      ;; we'll always resume at `timer-handler`
      (if (> current-ticks 0)
          ;; Next time this gets resumed, this process will be re-spawned
          ;; with the same ticks, complete routine and expire routine,
          ;; which is why resume->engine isn't necessary.
          (do-expire (lam (ticks)
                       (run resume ticks current-ticks complete expire)))
          ;; Found the process that ran out of time.
          ;; Again, the parent must time its child's expire routine.
          (begin (go parent-ticks)
                 (expire (resume->engine resume)))))))

(def (timer-handler)
  ;; Why `go`? Because we must resume the clock before returning to work.
  ;; In other words, we must transform the surrounding context to a 'resume'.
  (go (call/cc do-expire)))

;; This is the interface. Notice how the whole program logic
;; is wrapped in a procedure time-limited by `ticks`.
(def (proc->engine proc)
  (resume->engine
    (lam (ticks)
      ;; The engine starts the clock by itself.
      (go ticks)
      (let ([value (proc)])
        (let ([ticks-left (stop-timer)])
          (do-complete value ticks-left))))))




;; --------------------------------------
;; Concerning the stack
;; --------------------------------------
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





;; timed-lambda: The clock will be checked every time the
;; procedure is invoked.
(define-syntax-rule (tlam formals exp1 exp2 ...)
  (lam formals (check-timer) exp1 exp2 ...))
