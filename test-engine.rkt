#lang racket
(require "lang/kara.rkt"
         "engine.rkt")

(def eng
  (proc->engine (thunk 3)))

"thunk 3"
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
  (proc->engine
    (thunk
      (fib 10))))

(def (mileage thunk)
  (def (loop engine consumed-fuel)
    (engine 100
      ; Return ticks left
      (lam (value ticks)
        (+ consumed-fuel (- 100 ticks)))
      ; Keep looping
      (lam (new-eng)
        (loop new-eng (+ consumed-fuel 100)))))
  (loop (proc->engine thunk) 0))

"Mileage of fib"
; (mileage (thunk (fib 10)))

(def (print-even)
  (for-each (tlam (x) (printf "~s\n" x))
            (filter even? (range 0 100))))
(def even-engine (proc->engine print-even))

(def (print-odd)
  (for-each (tlam (x) (printf "~s\n" x))
            (filter odd? (range 1 100))))
(def odd-engine (proc->engine print-odd))

(def (round-robin engs)
  (if (null? engs)
      null
      (begin
        (printf "Switching\n")
        ((car engs) 1
           (lam (value ticks-left)
             (cons value (round-robin (cdr engs))))
           (lam (eng)
             (round-robin
               (lappend (cdr engs) (list eng))))))))

(def rr-engine
  (proc->engine (thunk
                 (round-robin (list even-engine odd-engine)))))

(def where-we-left-off null)
"Nesting engines"
(rr-engine 23
           "Done?"
           (lam (resume)
             (printf "It's not over\n")
             (set! where-we-left-off resume)))

(where-we-left-off 13
                   "Done?"
                   (lam (resume)
                     (printf "Now it's really over\n")
                     (set! where-we-left-off resume)))
