(library (types)
  (export i k p s)
  (import (chezscheme)
          (kara-lang main)
          (mol))

;;; Combinators
  (define i '(-> 0 0))

  (define k '(-> 0 (-> 1 0)))

  (define p '(=> (-> 0 1) 0 1))

  (define s
    '(-> (-> 0 (-> 1 2))
        (-> (-> 0 1)
           (-> 0 2))))

  (define ak
    '(=> (#f) (#f) ,k))

  (define as
    '(=> (#f) (#f) ,s))

  (define mp '(=> (=> 2 3 (-> 0 1))
                 (=> 4 5 0)
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
           (-> 0 2)))))
