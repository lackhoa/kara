#lang racket
(require "lang/kara.rkt"
         "constraint.rkt")

(def (adder a1 a2 sum)
  (def (process-new-value)
    (cond [(and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me)]
          [(and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me)]
          [(and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me)]))
  (def (me request)
    (cond [(eq? request 'I-have-a-value)
           (process-new-value)]
          [else (error "ADDER" "Unknown request" request)]))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(def A1 (make-connector))
(def A2 (make-connector))
(def SUM (make-connector))
(adder A1 A2 SUM)

(probe "A1" A1)
(probe "A2" A2)
(probe "SUM" SUM)
(set-value! SUM 200  'user)
(set-value! A1  160 'user)
