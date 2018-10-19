#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "enum.rkt"
         "types.rkt")
(provide (all-defined-out))


(def (collide reactor ort)
  ;; mol% -> [mol%] -> [mol%]
  ;; Returns ort, after colliding with reactor
  (for/fold
      ([new-ort  '()])
      ([orti     ort])
    (if (instance? orti reactor)
        new-ort
        (cons orti new-ort))))

(def (make-p fun arg)
  ;; mol% -> mol% -> mol%
  (>> (up p '[0] fun)
      (f> up '[1] arg)
      (f> ref '[2]  #|get conclusion|#)))

(def (combine reactor ort)
  ;; mol% -> [mol%] -> [mol%]  (new formulas)
  (for/fold
      ([accu  '()])
      ([orti  ort])
    `(,@(exclude-false `(,(make-p orti reactor)
                         ,(make-p reactor orti)))
      ,@accu)))
