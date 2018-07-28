#lang racket
(require "lang/kara.rkt"
         "timer.rkt")

(def (my-handler) "Job done!")
(start-timer 3 my-handler)
(decrement-timer)
(decrement-timer)
"Wait for it..."
(decrement-timer)
