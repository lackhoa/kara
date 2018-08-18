#lang racket
(require "lang/kara.rkt"
         "constraint.rkt")

(def (equate . equated)
  (when (exists has-value? equated)
    (let ([first-value
          (get-value (first-pass has-value? equated))])
      (for-each (lam (x) (set-value x first-value)) equated))))

(def (make-set-mem set member text)
  (def (process-new-value)
    (when (all-has-value? (list set member))
      (set-value text
                 (format "(in ~s ~s)"
                         (get-value set)
                         (get-value member)))))
  (def (me request)
    (cond [(eq? request 'I-have-a-value)
           (process-new-value)]
          [else (error "SET-MEM" "Unknown request" request)]))
  (connect set me)
  (connect member me)
  (connect text me))

(def (make-eq first second text)
  (def (process-new-value)
    (when (all-has-value? (list first second))
      (set-value text
                 (format "(= ~s ~s)"
                         (get-value first)
                         (get-value second)))))
  (def (me request)
    (cond [(eq? request 'I-have-a-value)
           (process-new-value)]
          [else (error "EQ" "Unknown request" request)]))
  (connect first me)
  (connect second me)
  (connect text me)
  me)


(def (reflexivity ante conse)
  (def (process-new-value)
    )
  (def (me request)
    (cond [(eq? request 'I-have-a-value)
           (process-new-value)]
          [else (error "ADDER" "Unknown request" request)]))
  (connect ante  me)
  (connect conse me)
  me)

(def LS  (make-connector))
(def SUM (make-connector))
(adder LS SUM)

(probe "LS" LS)
(probe "SUM" SUM)
(set-value! LS (list 1 2 3 99) 'user)
