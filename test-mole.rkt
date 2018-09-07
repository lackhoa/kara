#! /usr/bin/racket
#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt"
         rackunit)

;;; "Elementary stuff"
(def m1 (new mole%))

(send m1 update-role 'd W)

(def d (send m1 refr 'd))

(check-eq? (send d get-data)
           W)

(send m1 update-role 'a '?DATA)

(check-eqv? (length (send m1 get-roles))
            2)

(def a (send m1 refr 'a))

(check-equal? (send a get-sync-ls)
              (list a))

(send m1 update-path
  '[a-clone]
  '?DATA)

(def a-clone (send m1 refr 'a-clone))

(send a-clone sync a)

;; Now we update-path a
(send a
    update-path '[ad] X)
(def a-ad (send a refr 'ad))
(check-eq? (send a-ad get-data)
           X)

;; See what happens to a-clone
(def a-clone-ad (send a-clone refr 'ad))
(check-eq? (send a-clone-ad get-data)
           X)

;;; "another harder example"
(send a update-path '[b d] W)
(send a update-path '[c e] V)
(send a update-path '[c d] V)
(check-equal?
 (send (send a refr 'b) sync
   (send a refr 'c)
   (thunk "Will fail!"))
 "Will fail!")


;;; "More sync"
;; Let's go again with the sync
(send a update-path '[f g] V)

(send (send a refr 'b)
   sync (send a refr 'f) )

(displayln "These two should be the same")
(displayln (send a refr 'b))
(displayln (send a refr 'f))

;;; "Try modifying f from somewhere else"
(def m2 (new mole%))
(send m2 sync (send a refr 'b))
(send m2 update-path '[h i] T)

(displayln "These three should be the same")
(send a refr 'b)
m2
(send a refr 'f)

(send m2 update-path '[h q] R)
(check-equal? (send m2 update-path
                '[h q]
                Q
                (thunk "Now we fail"))
              "Now we fail")

(test-case
 "Sync from the upper level and change on the lower level"
 (def m3 (new mole%))
 (send m3 update-role 'tir X)
 (send m3 update-role 'tal '?DATA)
 (def m4 (new mole%))
 (send m4 update-role 'eth Y)
 (send m4 update-role 'el Z)
 (send m4 update-role 'tir '?DATA)
 (send m4 update-role 'tal '?DATA)
 (send m3 sync m4)
 (send m3
     update-role 'tal S)
 (check-eq? (send m4 refr-data 'tal)
            S))


(test-case
 "Cloning capability"
 (def test (new mole%))
 (def cp (send test copy))
 (check-equal? (send cp get-sync-ls)
               (list cp))
 (check-eq? (send test get-data)
            '?DATA)
 (send cp update-role 'j '?DATA)
 (send cp update-role 'k '?DATA)
 (def cpcp (send cp copy))
 ;; Now let's try the syncing
 (send (send cp refr 'j)
     sync (send cp refr 'k))
 (def cnop (send cp copy))
 (send cnop
     update-path '[j i] R)
 (check-eq? (send cnop ref-data '[j i])
            (send cnop ref-data '[k i])))

(test-case
 "Telekinesis"
 (def m1 (new mole%))
 (def m2 (new mole%))
 (send m1 update-role 'd '?DATA)
 (send m1 update-path '[d a] '?DATA)
 (send m1 update-path '[d b] '?DATA)
 (send m1 sync-path '[d a] '[d b])
 (send m1 sync m2)
 (def TELE (Sym TELE))
 (send m2 update-path '[d b] TELE)
 (check-eq? (send m1 ref-data '[d a])
            TELE)
 (check-eqv? (length (send (send m1 ref '[d a])
                         get-sync-ls))
             4))

(test-case
 "Intro to variables"
 (def m (new mole%))
 (displayln "This should be a variable")
 m)

(test-case
 "Intro to variables 2"
 (def m (new mole%))
 (send m sync-path '[a] '[b])
 (send m sync-path '[a] '[c d])
 (send m update-role 'e '?DATA)
 (displayln "a, b and d should  be the same")
 (pdisplay m 25))

(test-case
 "Hard variable example"
 (def m (new mole%))
 (send m sync-path '[a] '[b c])
 (send m sync-path '[e] '[b d])
 (displayln "a, c and d, e should  be the same")
 (pdisplay m 35))


(test-case
 "no-sync usage"
 (def m (new mole%))
 (send m distinguish-paths '([a] [b]))
 (check-equal? "Of course it fails!"
               (send m sync-path '[a] '[b] (thunk "Of course it fails!"))))

(test-case
 "no-sync usage 2"
 (def m (new mole%))
 (send m distinguish-paths '([a] [b]))
 (send m sync-path '[a] '[c])
 (check-equal? "Now it fails"
               (send m sync-path
                 '[c] '[b] (thunk "Now it fails"))))

(test-case
 "Display"
 (def m (new mole%))
 (send m update-role 'type wf)
 (displayln "This should also be a variable")
 m)

(test-case
 "Syncing with cycle"
 (def m (new mole%))
 (send m sync-path '[a b] '[c])
 (check-eq? #f
            (send m sync-path '[a] '[c] (thunk #f)))
 (check-eq? #f
            (send m sync-path '[a] '[a b] (thunk #f))))

(test-case
 "Syncing with cycle 2"
 (def m (new mole%))
 (send m sync-path '[a b] '[c])
 (check-eq? #f
            (send m sync-path '[] '[a] (thunk #f))))

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
