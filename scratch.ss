(define category
  (list (#|Equality meaning|#
         mk-proof '((= 0 1))
                  '(and (= (dom 0) (dom 1))
                      (= (codom 0) (codom 1))))

        (#|Equality meaning 2|#
         mk-proof '((= 0 2) (= 1 3))
                  '(= (compose 0 1) (compose 2 3)))

        (#|Identity|#
         mk-proof '((im 0))
                  '(and (= (compose 0 1) 0)
                      (= (compose 1 0) 0)))

        (#|Composition associativity|#
         mk-proof '()
                  '(= (compose 0 (compose 1 2))
                      (compose (compose 0 1) 2)))

        (#|Terminal object|#
         mk-proof '((term 2)
                    (= (codom 0) 2)
                    (= (codom 1) 2))
                  '(= 0 1))

        (#|There is a terminal object|#
         mk-proof '()
                  '(term (1)))

        (#|Isomorphism|#
         mk-proof '((isomorphic 0 1))
                  '(and (im (compose (iso 0 1)
                             (iso 1 0)))
                      (im (compose (iso 1 0)
                             (iso 0 1)))))

        (#|Isomorphism demand|#
         mk-proof '((im (compose 2 3))
                    (im (compose 3 2))
                    (= (dom 2) 0)
                    (= (dom 3) 1))
                  '(isomorphic 0 1))

        (#|Definition of elements|#
         mk-proof '((elem 0 2))
                  '(and (= (dom 0) (1))
                        (= (codom 0) 2)))

        (#|Definition of an empty object|#
         mk-proof '((elem 0 (empty)))
                  '(f))

        (#|There is an empty object|#
         mk-proof '()
                  '(empty))

        (#|There is an empty object|#
         mk-proof '()
                  '(empty))

        (#|Function|#
         mk-proof '((= (compose 0 (f))
                       (compose 1 (f))))
                  '(= 0 1))

        (#|Cartesian product|#
         mk-proof '((prod? 0 1 2 3 4))
                  '(and (= (compose 3 (f12 5 6)) 5)
                      (= (compose 4 (f12 5 6)) 6)))

        (#|Cartesian product, uniqueness|#
         mk-proof '((prod? 0 1 2 3 4)
                    (= (compose 3 7) 5)
                    (= (compose 4 7) 6))
                  '(= 7 (f12 5 6)))

        (#|Cartesian product exists|#
         mk-proof '()
                  '(prod? (prod 0 1 (pr1 0 1) (pr2 0 1))))))

(((Map i1 P1 X1)) ((Map i2 P2 X2))
 ((Map s1 X1 P1)) ((Map s2 X2 P2))
 ((Map t1 X1 P1)) ((Map t2 X2 P2))
 ((Map fa X1 X2)) ((Map fd P1 P2))

 ((= (c s1 i1) P1)) ((= (c s2 i2) P2))
 ((= (c t1 i1) P1)) ((= (c t2 i2) P2))

 ((= (c fd s1) (c s2 fa)))
 ((= (c fd s1) (c t2 fa)))
 ((= (c i2 fd) (c fa i1)))
 )
