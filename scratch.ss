;; I need to store the list of llg variables, they will all be freshed.
;; llg variables shall be more important than others
;; Should I make a framework of renaming? If a var is already freshed, then rename instead of unify.
;; CPS matche technique would be very useful!
;; The plan is to go from the list of states -> fetch the query variable -> anti-unify -> unify back in each clauses to get mini-substitutions -> utilize unify-command to get back the original semantics for each clause

(fresh (_.1 _.2 _.3)
  (== `((,_.1 . ,_.2) . ,_.3) env)
  (conde
   [;; Unify back in
    (== _.0 _.1)
    (== `(val . ,t0) _.2)]
   [(fresh (y1 z1)
      (== x1 _.1)
      (== `(rec . ,y1) _.2)
      (== `(closure ,y1 ((,x1 . (rec . ,y1) . ,z1))) t))]
   [(fresh (y2 z2 u2)
      (=/= x2 y2)
      ((lookupt x2 u2 v2) #t))]))

(x env t)
(((x0 ((x0 . (val . t0)) . z0) t0) () ())
 ((x1 ((x1 . (rec . y1)) . z1) (closure y1 ((x1 . (rec . y1)) . z1))) () ())
 ((x2 ((y2 . z2) . u2) v2) (((x2 y2))) ((lookupt x2 u2 v2) #t)))

((_.1 x1)
 (_.2 `(rec . ,y1))
 (_.3 z1)
 (_.4 `(closure ,y1 (()))))
;; Observation: the lhs is always an generalizer variable
;; The rhs is an arbitrary term of the original clause-based version

(_.0 ((_.1 . _.2) . _.3) _.4)
(((y0 (closure y1 ((x1 rec . y1) . z1)) v2) . _.4)
 ((z0 z1 u2)                                . _.3)
 (((val . y0) (rec . y1) z2)                . _.2)
 ((x0 x1 y2)                                . _.1)
 ((x0 x1 x2)                                . _.0))
(x env t)
(((x0 ((x0 . (val . t0)) . z0) t0) () ())
 ((x1 ((x1 . (rec . y1)) . z1) (closure y1 ((x1 . (rec . y1)) . z1))) () ())
 ((x2 ((y2 . z2) . u2) v2) (((x2 y2))) ((lookupt x2 u2 v2) #t)))
