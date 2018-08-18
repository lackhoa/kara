#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         rackunit)

"Elementary stuff"
(def m1
  (new mole% [data-i 'gaylord]))

(check-eq? (send m1 get-data)
           'gaylord)

(send m1 update-path '(d) 'W no-fail)

(def d (send m1 ref 'd))

(check-eq? (send d get-data) 'W)

(send m1
  update-path '(a) 'X no-fail)

(check-equal? (send m1 get-roles)
              '(a d))

(def a (send m1 ref 'a))

(check-equal? (send a get-data) 'X)

(check-equal? (send a get-sync-ls)
              (list a))

(send m1
  update-path '(a-clone)
         'UNKNOWN
         no-fail)

(def a-clone (send m1 ref 'a-clone))

(send a-clone
  sync a no-fail)

; Now we update-path a
(send a
  update-path '(ad) 'X no-fail)
(def a-ad (send a ref 'ad))
(check-eq? (send a-ad get-data)
           'X)

; See what happens to a-clone
(def a-clone-ad (send a-clone ref 'ad))
(check-eq? (send a-clone-ad get-data)
           'X)

"another harder example"
(send a
  update-path '(b) 'Y no-fail)
(send a
  update-path '(b d) 'W no-fail)
(send a
  update-path '(c) 'Z no-fail)
(send a
  update-path '(c e) 'V no-fail)
(check-equal?
 (send (send a ref 'b)
   sync (send a ref 'c) (lam () "Will fail!"))
 "Will fail!")


"More sync"
; Let's go again with the sync
(send a update-path '(f) 'Y no-fail)

(send a update-path '(f g) 'V no-fail)

(send (send a ref 'b)
   sync (send a ref 'f) no-fail)

(displayln "These two should be the same")
(send a ref 'b)
(send a ref 'f)

"Try modifying f from somewhere else"
(def m2 (new mole%))
(send m2 sync (send a ref 'b) no-fail)
(send m2 update-path '(h i) 'T no-fail)

(displayln "These three should be the same")
(send a ref 'b)
m2
(send a ref 'f)


"Fail update-path"
(send m2 update-path '(h) 'R no-fail)
(check-equal? (send m2 update-path '(h) 'RR (lam () "Now we fail"))
            "Now we fail")

(test-case
  "Sync from the upper level and change on the lower level"
  (def m3 (new mole%))
  (send m3 update-path '(tir) 'X no-fail)
  (send m3 update-path '(tal) 'UNKNOWN no-fail)
  (def m4 (new mole%))
  (send m4 update-path '(eth) 'Y no-fail)
  (send m4 update-path '(el) 'Z no-fail)
  (send m4 update-path '(tir) 'UNKNOWN no-fail)
  (send m4 update-path '(tal) 'UNKNOWN no-fail)
  (send m3 sync m4 no-fail)
  (send (send m3 ref 'tal)
    update-path null 'S no-fail)
  (check-eq? (send (send m4 ref 'tal) get-data)
             'S))


(test-case
  "Cloning capability"
  (def test (new mole%))
  (def cp (send test copy))
  (check-equal? (send cp get-sync-ls) (list cp))
  (send cp
    update 'L (lam () "Won't fail"))
  (check-eq? (send cp get-data) 'L)
  (check-eq? (send test get-data) 'UNKNOWN)
  (send cp update-path '(j) 'K no-fail)
  (def cpcp (send cp copy))
  (check-eq? (send (send cpcp ref 'j) get-data)
             'K)
  (send cp update-path '(k) 'K no-fail)
  ; Now let's try the syncing
  (send (send cp ref 'j)
    sync (send cp ref 'k) no-fail)
  (def cnop (send cp copy))
  (send cnop
    update-path '(j i) 'R no-fail)
  (check-eq? (send (send cnop ref '(j i)) get-data)
             (send (send cnop ref '(k i)) get-data)))

(test-case
  "Telekinesis"
  (def m1 (new mole%))
  (def m2 (new mole%))
  (send m1 update-path '(d) 'UNKNOWN no-fail)
  (send m1 update-path '(d a) 'UNKNOWN no-fail)
  (send m1 update-path '(d b) 'UNKNOWN no-fail)
  (send m1 sync-path '(d a) '(d b) no-fail)
  (send m1 sync m2 "Fuckit")
  (send m2 update-path '(d b) 'TELE no-fail)
  (check-eq? (send (send m1 ref '(d a)) get-data)
             'TELE)
  (check-eqv? (length (send (send m1 ref '(d a))
                        get-sync-ls))
              4))
