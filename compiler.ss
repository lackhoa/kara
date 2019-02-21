(define ==s
  (lambda (u v)
    (lambda (st0)
      (let* ([S0  (state-S st0)]
             [;; We know st must be an mzero (false) or a unit
              st  ((== u v) st0)])
        (cond
         [(eq? st (mzero)) #f]
         [else
          (let ([S (state-S st)])
            (if (eq? S0 S) (display "Useless!")
                (display "Cool!"))
            st)])))))
