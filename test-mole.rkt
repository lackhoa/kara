#! /usr/bin/racket
#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt"
         rackunit)

;;; "Elementary stuff"
(def m1 (new mole%))

(update-path m1 '[3] W)

(def d (send m1 refr 3))

(check-eq? (send d get-data)
           W)

(update-path m1 '[0] '?DATA)

(check-eqv? (length (send m1 get-roles))
            2)

(def a (send m1 refr 0))

(check-equal? (send a get-sync-ls)
              (list a))

(update-path m1 '[4] '?DATA)

(def a-clone (send m1 refr 4))

(send a-clone sync a)

;; Now we update-path a
(update-path a '[5] X)
(def a-ad (send a refr 5))
(check-eq? (send a-ad get-data)
           X)

;; See what happens to a-clone
(def a-clone-ad (send a-clone refr 5))
(check-eq? (send a-clone-ad get-data)
           X)

;;; "another harder example"
(update-path a '[0 0] W)
(update-path a '[1 0] V)
(check-equal?
 (sync-paths a '([0] [1])
             (thunk "Will fail!"))
 "Will fail!")


;;; "More sync"
;; Let's go again with the sync
(update-path a '[6 7] V)

(send (send a refr 1) sync
  (send a refr 6) )

(newline)
(displayln "These two should be the same")
(displayln (send a refr 1))
(displayln (send a refr 6))

;;; "Try modifying f from somewhere else"
(def m2 (new mole%))
(send m2 sync (send a refr 1))
(update-path m2 '[3 1] T)

(newline)
(displayln "These three should be the same")
(displayln (send a refr 1))
(displayln m2)
(displayln (send a refr 6))

(update-path m2 '[3 2] R)
(check-equal? (update-path m2 '[3 2] Q
                           (thunk "Now we fail"))
              "Now we fail")

(test-case
 "Sync from the upper level and change on the lower level"
 (def m3 (new mole%))
 (update-path m3 '[6] X)
 (update-path m3 '[10] '?DATA)
 (def m4 (new mole%))
 (update-path m4 '[8] Y)
 (update-path m4 '[3] Z)
 (update-path m4 '[6] '?DATA)
 (update-path m4 '[10] '?DATA)
 (send m3 sync m4)
 (update-path m3 '[10] S)
 (check-eq? (send (send m4 refr 10) get-data)
            S))


(test-case
 "Cloning capability"
 (def test (new mole%))
 (def cp (send test copy))
 (check-equal? (send cp get-sync-ls)
               (list cp))
 (check-eq? (send test get-data)
            '?DATA)
 (update-path cp '[5] '?DATA)
 (update-path cp '[2] '?DATA)
 (def cpcp (send cp copy))
 ;; Now let's try the syncing
 (send (send cp refr 5)
     sync (send cp refr 2))
 (def cnop (send cp copy))
 (update-path cnop '[5 3] R)
 (check-eq? (ref-data cnop '[5 3])
            (ref-data cnop '[2 3])))

(test-case
 "Telekinesis"
 (def m1 (new mole%))
 (def m2 (new mole%))
 (update-path m1 '[3] '?DATA)
 (update-path m1 '[4 0] '?DATA)
 (update-path m1 '[4 1] '?DATA)
 (sync-paths m1 '([4 0] [3 1]))
 (send m1 sync m2)
 (def TELE (Sym TELE))
 (update-path m2 '[4 1] TELE)
 (check-eq? (ref-data m1 '[4 1])
            TELE)
 (check-eqv? (length (send (ref m1 '[4 0]) get-sync-ls))
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
 (sync-paths m '([0] [1] [2 0]))
 (update-path m '[4] '?DATA)
 (newline)
 (displayln "0, 1 and 2-0 should  be the same")
 (pdisplay m 25))

(test-case
 "Hard variable example"
 (def m (new mole%))
 (sync-paths m '([0] [1 0]))
 (sync-paths m '([3] [2 1]))
 (newline)
 (displayln "0, 1-2 and 3, 2-1 should  be the same")
 (pdisplay m 35))


(test-case
 "no-sync usage"
 (def m (new mole%))
 (distinguish m '([0] [1]))
 (check-equal? "Of course it fails!"
               (sync-paths m '([0] [1]) (thunk "Of course it fails!"))))

(test-case
 "no-sync usage 2"
 (def m (new mole%))
 (distinguish m '([0] [1]))
 (sync-paths m '([0] [2]))
 (check-equal? "Now it fails"
               (sync-paths m
                           '([2] [1]) (thunk "Now it fails"))))

(test-case
 "Syncing with cycle"
 (def m (new mole%))
 (sync-paths m '([0 1] [2]))
 (check-eq? #f
            (sync-paths m '([0] [2]) (thunk #f)))
 (check-eq? #f
            (sync-paths m '([0] [0 1]) (thunk #f))))

(test-case
 "Syncing with cycle 2"
 (def m (new mole%))
 (sync-paths m '([0 1] [2]))
 (check-eq? #f
            (sync-paths m '([] [0]) (thunk #f))))

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
 (update-path m '[0] '?DATA)
 (preserve m '([0] [1]))
 (update-path m '[1] '?DATA)
 (distinguish m '([0] [1]))

 (def mc (send m copy))
 (check-equal? (send (send mc refr 1) get-no-sync)
               (list (send mc refr 0)))
 (check-eq? #f
            (update-path mc '[1] R
                         (thunk #f))))

(test-case
 "Replaceability"
 (def m (new mole%))
 (update-path m '[0] A)
 (update-path m '[0 0] B)
 (def mr (new mole%))
 (update-path mr '[0] A)
 (check-eq? (replaceable? m mr) #t "m can be replaced")
 (check-eq? (replaceable? mr m) #f "mr cannot be replaced")
 (check-eq? (replaceable? m m)  #t "m can be replaced by itself"))
