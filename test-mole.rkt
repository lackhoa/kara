#! /usr/bin/racket
#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt"
         rackunit)

;;; "Elementary stuff"
(def m1 (new mole%))

(send m1 update-role 3 W)

(def d (send m1 refr 3))

(check-eq? (send d get-data)
           W)

(send m1 update-role 0 '?DATA)

(check-eqv? (length (send m1 get-roles))
            2)

(def a (send m1 refr 0))

(check-equal? (send a get-sync-ls)
              (list a))

(send m1 update-path
  '[4]
  '?DATA)

(def a-clone (send m1 refr 4))

(send a-clone sync a)

;; Now we update-path a
(send a update-path
  '[5] X)
(def a-ad (send a refr 5))
(check-eq? (send a-ad get-data)
           X)

;; See what happens to a-clone
(def a-clone-ad (send a-clone refr 5))
(check-eq? (send a-clone-ad get-data)
           X)

;;; "another harder example"
(send a update-path '[0 0] W)
(send a update-path '[1 0] V)
(check-equal?
 (send a sync-paths
   '([0] [1])
   (thunk "Will fail!"))
 "Will fail!")


;;; "More sync"
;; Let's go again with the sync
(send a update-path '[6 7] V)

(send (send a refr 1) sync
  (send a refr 6) )

(newline)
(displayln "These two should be the same")
(displayln (send a refr 1))
(displayln (send a refr 6))

;;; "Try modifying f from somewhere else"
(def m2 (new mole%))
(send m2 sync (send a refr 1))
(send m2 update-path '[3 1] T)

(newline)
(displayln "These three should be the same")
(displayln (send a refr 1))
(displayln m2)
(displayln (send a refr 6))

(send m2 update-path '[3 2] R)
(check-equal? (send m2 update-path
                '[3 2]
                Q
                (thunk "Now we fail"))
              "Now we fail")

(test-case
 "Sync from the upper level and change on the lower level"
 (def m3 (new mole%))
 (send m3 update-role 6 X)
 (send m3 update-role 10 '?DATA)
 (def m4 (new mole%))
 (send m4 update-role 8 Y)
 (send m4 update-role 3 Z)
 (send m4 update-role 6 '?DATA)
 (send m4 update-role 10 '?DATA)
 (send m3 sync m4)
 (send m3
     update-role 10 S)
 (check-eq? (send m4 refr-data 10)
            S))


(test-case
 "Cloning capability"
 (def test (new mole%))
 (def cp (send test copy))
 (check-equal? (send cp get-sync-ls)
               (list cp))
 (check-eq? (send test get-data)
            '?DATA)
 (send cp update-role 5 '?DATA)
 (send cp update-role 2 '?DATA)
 (def cpcp (send cp copy))
 ;; Now let's try the syncing
 (send (send cp refr 5)
     sync (send cp refr 2))
 (def cnop (send cp copy))
 (send cnop
     update-path '[5 3] R)
 (check-eq? (send cnop ref-data '[5 3])
            (send cnop ref-data '[2 3])))

(test-case
 "Telekinesis"
 (def m1 (new mole%))
 (def m2 (new mole%))
 (send m1 update-role 3 '?DATA)
 (send m1 update-path '[4 0] '?DATA)
 (send m1 update-path '[4 1] '?DATA)
 (send m1 sync-paths '([4 0] [3 1]))
 (send m1 sync m2)
 (def TELE (Sym TELE))
 (send m2 update-path '[4 1] TELE)
 (check-eq? (send m1 ref-data '[4 1])
            TELE)
 (check-eqv? (length (send (send m1 ref '[4 0]) get-sync-ls))
             4))

(test-case
 "Intro to variables"
 (def m (new mole%))
 (newline)
 (displayln "This should be a variable")
 (displayln m))

(test-case
 "Intro to variables 2"
 (def m (new mole%))
 (send m sync-paths '([0] [1] [2 0]))
 (send m update-role '4 '?DATA)
 (newline)
 (displayln "0, 1 and 2-0 should  be the same")
 (pdisplay m 25))

(test-case
 "Hard variable example"
 (def m (new mole%))
 (send m sync-paths '([0] [1 0]))
 (send m sync-paths '([3] [2 1]))
 (newline)
 (displayln "0, 1-2 and 3, 2-1 should  be the same")
 (pdisplay m 35))


(test-case
 "no-sync usage"
 (def m (new mole%))
 (send m distinguish '([0] [1]))
 (check-equal? "Of course it fails!"
               (send m sync-paths '([0] [1]) (thunk "Of course it fails!"))))

(test-case
 "no-sync usage 2"
 (def m (new mole%))
 (send m distinguish '([0] [1]))
 (send m sync-paths '([0] [2]))
 (check-equal? "Now it fails"
               (send m sync-paths
                 '([2] [1]) (thunk "Now it fails"))))

(test-case
 "Syncing with cycle"
 (def m (new mole%))
 (send m sync-paths '([0 1] [2]))
 (check-eq? #f
            (send m sync-paths '([0] [2]) (thunk #f)))
 (check-eq? #f
            (send m sync-paths '([0] [0 1]) (thunk #f))))

(test-case
 "Syncing with cycle 2"
 (def m (new mole%))
 (send m sync-paths '([0 1] [2]))
 (check-eq? #f
            (send m sync-paths '([] [0]) (thunk #f))))

(test-case
 "No touch"
 (def m (new mole%))
 (send m mark-no-touch)
 (check-eq? #f
            (send m update A (thunk #f))))

(test-case
 "No touch 2"
 (def m1 (new mole%))
 (def m2 (new mole%))
 (send m1 sync m2)
 (send m1 mark-no-touch)
 (check-eq? #f
            (send m2 update A (thunk #f))))

(test-case
 "clone will preserve no-sync and no-touch"
 (def m (new mole%))
 (send m update-role 0 '?DATA)
 (send m preserve '([0] [1]))
 (send m update-role 1 '?DATA)
 (send m distinguish '([0] [1]))

 (def mc (send m copy))
 (check-equal? (send (send mc refr 1) get-no-sync)
               (list (send mc refr 0)))
 (check-eq? #f
            (send mc update-role 1 R (thunk #f))))
