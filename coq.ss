;; (define make-obj (lambda (type val) `(type val)))
;; (define obj->type car)
;; (define obj->val cadr)

(define !-
  (lambda (g exp t)
    (conde
     [(var-rel g exp t)]
     [(int-rel g exp t)]
     [(bool-rel g exp t)]
     [(zero?-rel g exp t)]
     [(sub1-rel g exp t)]
     [(fix-rel g exp t)]
     [(*-rel g exp t)]
     [(lambda-rel g exp t)]
     [(app-rel g exp t)]
     [(if-rel g exp t)]
     )))

(define int-rel
  (lambda (g exp t)
    (fresh ()
      (numbero exp)
      (== 'int t))))

(define bool-rel
  (lambda (g exp t)
    (fresh ()
      (conde [(== #t exp)] [(== #f exp)])
      (== 'bool t))))

(define zero?-rel
  (lambda (g exp t)
    (fresh (e)
      (== `(zero? ,e) exp)
      (== 'bool t)
      (!- g e 'int))))

(display "Hello")
(define sub1-rel
  (lambda (g exp t)
    (fresh (e)
      (== `(sub1 ,e) exp)
      (== 'int t)
      (!- g e 'int))))
(display "Hello")

(define *-rel
  (lambda (g exp t)
    (fresh (e1 e2)
      (== `(* ,e1 ,e2) exp)
      (== 'int t)
      (!- g e1 'int)
      (!- g e2 'int))))

(define if-rel
  (lambda (g exp t)
    (fresh (test conseq alt)
      (== `(if ,test ,conseq ,alt) exp)
      (!- g test 'bool)
      (!- g conseq t)
      (!- g alt t))))

(define fix-rel
  (lambda (g exp t)
    (fresh (rand)
      (== `(fix ,rand) exp)
      (!- g rand `(-> ,t ,t)))))

(define var-rel
  (lambda (g exp t)
    (fresh ()
      (symbolo exp)
      (lookupo exp t g))))

(define lambda-rel
  (lambda (g exp t)
    (fresh (rand body trand tbody)
      (== `(lambda ,rand ,body) exp)
      (== `(-> ,trand ,tbody) t)
      (!- `((,rand ,trand) . ,g) body tbody))))

(define app-rel
  (lambda (g exp t)
    (fresh (rator rand trand)
      (== `(,rator ,rand) exp)
      (!- g rator `(-> ,trand ,t))
      (!- g rand trand))))

(define lookupo
  (lambda (x tx g)
    (fresh (a d)
      (== `(,a . ,d) g)
      (conde
       [(== `(,x ,tx) a)]
       [(fresh (y _ty)
          (== `(,y ,_ty) a)
          (=/= x y)
          (lookupo x tx d))]))))
