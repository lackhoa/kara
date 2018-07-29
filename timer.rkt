#lang racket
(require "lang/kara.rkt")
(provide start-timer stop-timer check-timer)

; ; `null` means the clock is not on
(def clock 0)
(def expire-handler #f)

(def (start-timer initial-ticks new-expire-handler)
  (set! expire-handler new-expire-handler)
  (set! clock initial-ticks))

(def (stop-timer)
  (let ([time-left clock])
    (set! clock 0)
    time-left))

; Only effective when the clock is used
(def (check-timer)
  (when (> clock 0)
    (set! clock (- clock 1))
    (when (= clock 0)
      (expire-handler))))

