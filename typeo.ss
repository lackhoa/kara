(define init-ctx '())

(define lookupo
  (lambda (x tx ctx)
    (fresh (a d)
      (== `(,a . ,d) ctx)
      (conde
       [(== `(,x ,tx) a)]
       [(fresh (y _ty)
          (== `(,y ,_ty) a)
          (=/= x y)
          (lookupo x tx d))]))))

(define typeo
  ;; Type checking relation
  (lambda (ctx exp t)
    (conde
     [(var-rel ctx exp t)]
     [(int-rel ctx exp t)]
     [(bool-rel ctx exp t)]
     [(lambda-rel ctx exp t)]
     [(app-rel ctx exp t)])))

(define int-rel
  (lambda (ctx exp t)
    (fresh ()
      (numbero exp)
      (== 'int t))))

(define bool-rel
  (lambda (ctx exp t)
    (fresh ()
      (conde [(== #t exp)] [(== #f exp)])
      (== 'bool t))))

(define var-rel
  (lambda (ctx exp t)
    (fresh ()
      (symbolo exp)
      (lookupo exp t ctx))))

(define lambda-rel
  (lambda (ctx exp t)
    (fresh (rand trand body trand tbody)
      (== `(lambda (,rand ,trand) ,body) exp)
      (== `(Pi (,rand ,trand) ,tbody) t)
      (typeo `((,rand ,trand) . ,ctx) body tbody))))

(define app-rel
  (lambda (ctx exp t)
    (fresh (rator rand trand)
      (== `(,rator ,rand) exp)
      (typeo ctx rator `(-> ,trand ,t))
      (typeo ctx rand trand))))
