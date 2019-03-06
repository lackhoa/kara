(define nat
  '(inductive Set tt))

(define ind
  (lambda (type)
    (pmatch type
      [(inductive ,)])))
