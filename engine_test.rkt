#lang racket
(require "lang/kara.rkt"
         "engine.rkt")

(def eng
  (make-engine (tlam () 3)))

(eng 8
  ; Complete: Return ticks remaining and value.
  list
  ; Expire: Return ticks remaining.
  (lam (x) x))

(def (fib n)
  (check-timer)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(def fib-eng
  (make-engine
    (lam ()
      (fib 10))))

; (fib-eng 100
;   list
;   (lam (new-eng)
;     (set! fib-eng new-eng)
;     "Expired!"))

; (fib-eng 100
;   list
;   (lam (new-eng)
;     (set! fib-eng new-eng)
;     "Expired!"))

(def (mileage thunk)
  (def (loop engine consumed-fuel)
    (engine 100
      ; Return ticks left
      (lam (value ticks) (+ consumed-fuel (- 100 ticks)))
      ; Keep looping
      (lam (new-eng)
        (loop new-eng (+ consumed-fuel 100)))))
  (loop (make-engine thunk) 0))

(mileage (lam () (fib 10)))


