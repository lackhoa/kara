#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         rackunit)

"Elementary stuff"
(def m1
  (new mole% [data-i 'gaylord]))

(check-eq? (send m1 get-data)
           'gaylord)

(send m1 update-path '(d) 'W)

(def d (send m1 refr 'd))

(check-eq? (send d get-data) 'W)

(send m1
  update-path '(a) 'X)

(check-equal? (send m1 get-roles)
              '(a d))

(def a (send m1 refr 'a))

(check-equal? (send a get-data) 'X)

(check-equal? (send a get-sync-ls)
              (list a))

(send m1
  update-path '(a-clone)
         'UNKNOWN
        )

(def a-clone (send m1 refr 'a-clone))

(send a-clone
  sync a)

; Now we update-path a
(send a
  update-path '(ad) 'X)
(def a-ad (send a refr 'ad))
(check-eq? (send a-ad get-data)
           'X)

; See what happens to a-clone
(def a-clone-ad (send a-clone refr 'ad))
(check-eq? (send a-clone-ad get-data)
           'X)

"another harder example"
(send a
  update-path '(b) 'Y)
(send a
  update-path '(b d) 'W)
(send a
  update-path '(c) 'Z)
(send a
  update-path '(c e) 'V)
(check-equal?
 (send (send a refr 'b)
   sync (send a refr 'c) (lam () "Will fail!"))
 "Will fail!")


"More sync"
; Let's go again with the sync
(send a update-path '(f) 'Y)

(send a update-path '(f g) 'V)

(send (send a refr 'b)
   sync (send a refr 'f))

(displayln "These two should be the same")
(send a refr 'b)
(send a refr 'f)

"Try modifying f from somewhere else"
(def m2 (new mole%))
(send m2 sync (send a refr 'b))
(send m2 update-path '(h i) 'T)

(displayln "These three should be the same")
(send a refr 'b)
m2
(send a refr 'f)


"Fail update-path"
(send m2 update-path '(h) 'R)
(check-equal? (send m2 update-path '(h) 'RR (lam () "Now we fail"))
            "Now we fail")

(test-case
  "Sync from the upper level and change on the lower level"
  (def m3 (new mole%))
  (send m3 update-path '(tir) 'X)
  (send m3 update-path '(tal) 'UNKNOWN)
  (def m4 (new mole%))
  (send m4 update-path '(eth) 'Y)
  (send m4 update-path '(el) 'Z)
  (send m4 update-path '(tir) 'UNKNOWN)
  (send m4 update-path '(tal) 'UNKNOWN)
  (send m3 sync m4)
  (send (send m3 refr 'tal)
    update-path null 'S)
  (check-eq? (send (send m4 refr 'tal) get-data)
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
  (send cp update-path '(j) 'K)
  (def cpcp (send cp copy))
  (check-eq? (send (send cpcp refr 'j) get-data)
             'K)
  (send cp update-path '(k) 'K)
  ; Now let's try the syncing
  (send (send cp refr 'j)
    sync (send cp refr 'k))
  (def cnop (send cp copy))
  (send cnop
    update-path '(j i) 'R)
  (check-eq? (send (send cnop ref '(j i)) get-data)
             (send (send cnop ref '(k i)) get-data)))

(test-case
  "Telekinesis"
  (def m1 (new mole%))
  (def m2 (new mole%))
  (send m1 update-path '(d) 'UNKNOWN)
  (send m1 update-path '(d a) 'UNKNOWN)
  (send m1 update-path '(d b) 'UNKNOWN)
  (send m1 sync-path '(d a) '(d b))
  (send m1 sync m2)
  (send m2 update-path '(d b) 'TELE)
  (check-eq? (send (send m1 ref '(d a)) get-data)
             'TELE)
  (check-eqv? (length (send (send m1 ref '(d a))
                        get-sync-ls))
              4))

(test-case
  "Mutation"
  (def m1 (new mole%))
  (def m2 (new mole%))
  (send m1 update 'M)
  (check-eq? (send m1 get-data)
             'M)
  (send m2 sync m1)
  (send m2 mutate 'CHANGED)
  (check-eq? (send m1 get-data)
             'CHANGED))
