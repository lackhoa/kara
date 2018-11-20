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

(=> (ap-core (bind (bind (*))) 0 (bind (bind (const 0)))))
3 steps

(=> (ap-core (bind (bind (* * . 0))) 1 (bind (bind (* . 0)))))
3 steps

(=> (ap-core (bind (bind (const 0))) 1 (bind (bind (const 0)))))
3 steps
(=> (ap-core (bind (*)) 0 (bind (const 0))))
2 steps
(=> (ap-core (bind (* * . 0)) 1 (bind (* . 0))))
2 steps
(=> (ap-core (bind (const 0)) 1 (bind (const 0))))
2 steps
(=> (ap-core (*) 0 (const 0)))
1 steps
(=> (ap-core (* * . 0) 1 (* . 0)))
1 steps
(=> (ap-core (const 0) 1 (const 0)))
1 steps
(=> (ap-core (:: (*) (*)) 0 (:: (const 0) (const 0))))
3 steps
(=> (ap-core (:: (*) (* * . 0)) 1 (:: (const 1) (* . 0))))
3 steps
(=> (ap-core (:: (*) (const 0)) 1 (:: (const 1) (const 0))))
3 steps
(=> (ap-core (:: (* * . 0) (*)) 1 (:: (* . 0) (const 1))))
3 steps
(=> (ap-core (:: (* * . 0) (* * . 1)) 2 (:: (* . 0) (* . 1))))
3 steps
(=> (ap-core (:: (* * . 0) (const 1)) 2 (:: (* . 0) (const 1))))
3 steps
(=> (ap-core (:: (const 0) (*)) 1 (:: (const 0) (const 1))))
3 steps
(=> (ap-core (:: (const 0) (* * . 1)) 2 (:: (const 0) (* . 1))))
3 steps
(=> (ap-core (:: (const 0) (const 1)) 2 (:: (const 0) (const 1))))
3 steps
(=> (apply (bind (bind (*))) 0 (bind (const 0))))
3 steps
(=> (apply (bind (bind (* * . 0))) 1 (bind (* . 0))))
3 steps
(=> (apply (bind (bind (const 0))) 1 (bind (const 0))))
3 steps
(=> (apply (bind (*)) 0 (const 0)))
2 steps
(=> (apply (bind (* * . 0)) 1 (* . 0)))
2 steps
(=> (apply (bind (const 0)) 1 (const 0)))
2 steps
(=> (decode (const 0) 0))
1 steps
(=> (decode (:: (const 0) (const 1)) (0 . 1)))
3 steps
(=> (decode (bind (*)) 0))
3 steps
(=> (decode (bind (const 0)) 0))
3 steps
