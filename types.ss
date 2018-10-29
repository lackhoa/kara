(library (types)
  (export i k s b c as ak mp p
          ent% ent-prem ent-ccs
          equality category)
  (import (chezscheme)
          (kara-lang main)
          (mol))

;;; Combinators
  (define ent%
    (lambda (prem concl)
      (let ([new-number!
             (let ([i 0])
               (lambda () (begin (set! i (+ i 1))
                            (* i 100))))])
        `(=> (pre ,@(map (lambda (p) `(=> ,(new-number!) ,p))
                        prem  #|Gives each premiss its own bogus premiss|#))
            ,concl))))

  (define ent-prem
    (lambda (ent)
      (mol-< (ref ent '[0])
             (lambda (_) #f  #|Be careful! Premise is not yet specified|#)
             (lambda (_ kids) kids))))

  (define ent-ccs
    (f> ref '[1]))

  (define i '(-> 0 0))

  (define k '(-> 0 (-> 1 0)))

  (define s
    '(-> (-> 0 (-> 1 2))
        (-> (-> 0 1)
           (-> 0 2))))

  (define p '(=> (-> 0 1) 0 1))

  (define ak
    (ent% '() k))

  (define as
    (ent% '() s))

  (define mp
    (ent% '((-> 0 1) 0)
          1))


;;; Some more combinators
  (define w
    '(-> (-> 0 (-> 0 1))
        (-> 0 1)))

  (define c
    '(-> (-> 0 (-> 1 2))
        (-> 1 (-> 0 2))))

  (define b
    '(-> (-> 1 2)
        (-> (-> 0 1)
           (-> 0 2))))

  (define equality
    (list (ent% '()
                '(= 0 0))

          (ent% '((= 1 0))
                '(= 0 1))

          (ent% '((= 0 1) (= 1 2))
                '(= 0 2))))

  (define category
    (list (ent% '((morph 0 1 2) (morph 3 2 4))
                '(morph (comp 3 0) 1 4)  #|Composition type|#)

          (ent% '((im 1) (morph 1 0 2))
                '(= 0 2)  #|Type of identity morphism|#)

          (ent% '((= 0 1) (morph 0 2 3) (morph 1 4 5))
                '(= 2 4))

          (ent% '((= 0 1) (morph 0 2 3) (morph 1 4 5))
                '(= 3 5))

          (ent% '((im 0))
                '(= (comp 0 1) 0)  #|Left identity|#)

          (ent% '((im 0))
                '(= (comp 1 0) 0)  #|Right identity|#)
          ))
  )
