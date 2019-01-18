(define assoc-ops '(c * +))
(define commu-ops '(* +))

(define size
  (lambda (x)
    (cond [(var? x)   1]
          [(pair? x)  (+ (size (car x))
                         (size (cdr x)))]
          [(null? x)  0]
          [else       1])))

(define ->
  ;; Encode rules: right now let's just use one
  ;; T is the encompassing formula within which t transform to t+
  ;; T+ is just T after the transformation
  (lambda (t t+)
    (fresh (l r y)
      (;; dirty "conda": only return all answers from one branch
       conda [(== t `(not (+ ,l ,r)))]
             [(== t `(not (+ ,r ,l)))])
      (conda [(== l `(not (+ ,t+ ,y)))]
             [(== l `(not (+ ,y ,t+)))])
      (conda [(== r `(not (+ ,t+ (not ,y))))]
             [(== r `(not (+ (not ,y) ,t+)))]))))

(define micro
  ;; Step* on all operands
  (lambda (start end)
    (conde [(symbolo start) (== start end)]
           [(fresh (rator rands rands+)
              (== start (cons rator rands))
              (mapo macro rands rands+))])))

(define macro
  ;; Step* on the formula only
  (lambda (start end)
    (conde [(== start end)]
           [(=/= start end)
            (fresh (start+ t t+ t++)
              (conde [;; Symbol & Negation
                      (conde [(symbolo start)]
                             [(fresh (x) (== start `(not ,x)))])
                      (== t start)]
                     [;; Disjunction
                      (fresh (disj disj+ R)
                        (== start (cons '+ disj))
                        (select-disj t disj R)
                        (add-disj t++ R disj+)
                        (== start+ (cons '+ disj+)))])
              (<-> t t+)
              (micro t+ t++)
              (;; Recursive search: we don't micro here, since we
               ;; already did that on both old and new operands
               macro start+ end))])))
