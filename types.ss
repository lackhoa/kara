(library (types)
  (export i k p s b c as ak mp
          equality ent% ent-prem ent-ccs)
  (import (chezscheme)
          (kara-lang main)
          (mol))

;;; Combinators
  (define ent%
    (lambda (prem concl)
      `(=> (pre ,@prem) ,concl)))

  (define ent-prem
    (lambda (ent)
      (mol-< (ref ent '[0])
             (lambda (_) #f  #|Be careful!|#)
             (lambda (_ kids) kids))))

  (define ent-ccs
    (f> ref '[1]))

  (define i '(-> 0 0))

  (define k '(-> 0 (-> 1 0)))

  (define p '(=> (-> 0 1) 0 1))

  (define s
    '(-> (-> 0 (-> 1 2))
        (-> (-> 0 1)
           (-> 0 2))))

  (define ak
    `(=> (pre) ,k))

  (define as
    `(=> (pre) ,s))

  (define mp '(=> (pre (=> 100 (-> 0 1))
                      (=> 200 0))
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
    '((=> (pre) (= 0 0))

      (=> (pre (=> 100 (= 1 0)))
         (= 0 1))

      (=> (pre (=> 100 (= 0 1))
              (=> 200 (= 1 2)))
         (= 0 2))))
  )
