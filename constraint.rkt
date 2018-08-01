#lang racket
(require "lang/kara.rkt")

(provide (all-defined-out))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(def (constant value connector)
  (def (me request)
    (error "Constant" "Unknown request" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(def (probe name connector)
  (def (process-new-value)
    (printf "\nProbe: ~s = ~s\n"
            name
            (get-value connector)))
  (def (me request)
    (cond [(eq? request 'I-have-a-value)
           (process-new-value)]
          [else (error "PROBE" "Unknown request" request)]))
  (connect connector me)
  me)

(def (make-connector)
  (let ([value false]
        [informant false]
        [constraints null])

    (def (set-my-value new-val setter)
      (cond [(not (has-value? me))
             (set! value new-val)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints)]
            [(not (= value new-val))
             (error "CONNECTOR" "Contradiction" (list value new-val))]
            [else 'ignored]))

    (def (connect new-constraint)
      (when (not (memq new-constraint constraints))
            (set! constraints
                  (cons new-constraint constraints)))
      (when (has-value? me)
            (inform-about-value new-constraint))
      'done)

    (def (me request)
      (cond [(eq? request 'has-value?)
             (if informant true false)]
            [(eq? request 'value) value]
            [(eq? request 'set-value!) set-my-value]
            [(eq? request 'connect) connect]
            [else (error "CONNECTOR" "Unknown operation" request)]))
    me))

(def (for-each-except exception procedure list)
  (let loop ([items list])
    (cond [(null? items) 'done]
          [(eq? (car items) exception)
           (loop (cdr items))]
          [else (procedure (car items))
                (loop (cdr items))])))

(def (has-value? connector)
  (connector 'has-value?))

(def (get-value connector)
  (connector 'value))

(def (set-value! connector new-val informant)
  ((connector 'set-value!) new-val informant))

(def (connect connector new-constraint)
  ((connector 'connect) new-constraint))
