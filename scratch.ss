(define alpha-equiv?
  (lambda (e1 e2)
    (let alpha ([e1 e1] [e2 e2]
            [xs '()] [ys '()])
      (pmatch `(,e1 ,e2)
        [(,x ,y)
         (guard (var? x) (var? y))
         (pmatch `(,(assq x xs) ,(assq y ys))
           [(#f #f)           (eq? x y)]
           [((? ,b1) (? ,b2)) (eq? b1 b2)]
           [(? ?)             #f])]
        [((forall ,x ,b1) (forall ,y ,b2))
         (let ([fresh (gensym)])
           (let ([xs `((,x ,fresh) ,@xs)]
                 [ys `((,y ,fresh) ,@ys)])
             (alpha b1 b2 xs ys)))]
        [((,fun1 . ,args1) (,fun2 . ,args2))
         (and (eq? fun1 fun2)
              (= (length args1) (length args2))
              (let ([alpha (lambda (arg1 arg2) (alpha arg1 arg2 xs ys))])
                (andmap alpha args1 args2)))]
        [(,c1 ,c2) (eq? c1 c2)]))))

(define pat-match
  (lambda (pat term)
    (let pat-match ([pat pat] [term term] [S '()])
      (cond
       [(var? pat)
        (pmatch (assq pat S)
          [(,pat ,t) (and (equal? term t) S)]
          [#f `((,pat ,term) ,@S)])]
       [(and (pair? pat) (pair? term))
        (let ([S (pat-match (car pat) (car term) S)])
          (and S (pat-match (cdr pat) (cdr term) S)))]
       [(eq? pat term) S]
       [else #f]))))

(define fill
  (lambda (pat S)
    (let fill ([pat pat])
      (cond
       [(var? pat)
        (pmatch (assq pat S)
          [(,pat ,res) res]
          [else (error 'fill "A weird situation!" pat S)])]
       [(pair? pat)
        `(,(fill (car pat)) ,@(fill (cdr pat)))]
       [else pat]))))

(define rewrite-outer
  (lambda (f g)
    (let rewrite-outer ([g g])
      (pmatch g
        [(forall ? ,g) `(forall ,x ,(rewrite-outer g))]
        [(-> ,f ,g) `(-> ,(rewrite-outer f)
                       ,(rewrite-outer g))]
        [(,pred . ,args)
         (let ([rule (rip f)])
           (pmatch rule
             [(= ,l ,r)
              (rewrite-core l r `(,pred
                                  ,@(map (lambda (t) (rewrite-core t))
                                         args)))]
             [else (error 'rewrite "Not a rewrite rule" rule)]))]))))

(define rewrite-core
  (lambda (l r t)
    (let rewrite-core ([t t])
      (let ([S (pat-match l t)])
        (let ([t (if S (fill r S) t)])
          (pmatch t
            [(,fun . ,args) `(,fun ,@(map rewrite-core args))]
            [,x x]))))))

(define rewrite
  (lambda (f)
    (lambda (env)
      (let ([g (local-g env)])
        ((go (assert (rewrite-core f g))
             done)
         env)))))

(define rip (lambda (f) (pmatch f [(forall ,? ,g) (rip g)] [else f])))
