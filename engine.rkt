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

(def (run resume parent-ticks child-ticks complete expire)
  (let ([ticks
         (if (and (active?) (< parent-ticks child-ticks))
             parent-ticks
             child-ticks)])
    ; One of the residual tick values will be 0.
    (push (- parent-ticks ticks) (- child-ticks ticks) complete expire)
    (resume ticks)))

(def (go ticks)
  (when (active?)
    (if (= ticks 0)
        (expire-handler)
        (start-timer ticks expire-handler))))

(def (do-complete value ticks-left)
  (pop (lam (parent-ticks child-ticks complete expire)
         (go (+ parent-ticks ticks-left))
         (complete value (+ child-ticks ticks-left)))))

(def (do-expire resume)
  (pop (lam (parent-ticks child-ticks complete expire)
         (if (> child-ticks 0)
             (do-expire (lam (ticks)
                          (run resume ticks child-ticks complete expire)))
             (begin (go parent-ticks)
                    (expire (new-engine resume)))))))

(def (expire-handler)
  (go (call/cc do-expire)))

; This is the interface.
(def (make-engine proc)
  (new-engine
    (lam (ticks)
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
