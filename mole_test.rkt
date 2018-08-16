#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         rackunit)

(def m1
  (new mole% [data-i 'gaylord]))

(check-eq? (send m1 get-data)
           'gaylord)

(send m1 update '(d) 'W (lam () "ain't gonna fail"))

(def d (send m1 ref 'd))

(check-eq? (send d get-data) 'W)

(send m1
  update '(a) 'X (lam () "How the heck did I fail?"))

(check-equal? (send m1 get-roles)
              '(a d))

(def a (send m1 ref 'a))

(check-equal? (send a get-data) 'X)

(check-equal? (send a get-sync-ls)
              (list a))

(send m1
  update '(a-clone)
         'UNKNOWN
         (lam () "That can't fail!"))

(def a-clone (send m1 ref 'a-clone))

(send a-clone
  sync a (lam () "Won't fail, for sure"))

; Now we update a
(send a
  update '(ad) 'X (lam () "Won't fail"))

(def a-ad (send a ref 'ad))

(check-eq? (send a-ad get-data)
           'X)

; See what happens to a-clone
(def a-clone-ad (send a-clone ref 'ad))
(check-eq? (send a-clone-ad get-data)
           'X)

; adnother harder example:
(send a update '(b) 'Y (lam () "Impossible"))

(send a update '(b d) 'W (lam () "Definitely"))

(send a update '(c) 'Z (lam () "No fail"))

(send a update '(c e) 'V (lam () "Nope"))

(check-equal?
 (send (send a ref 'b)
   sync (send a ref 'c) (lam () "Will fail!"))
 "Will fail!")

; Let's go again with the sync
(send a update '(f) 'Y (lam () "No fail"))

(send a update '(f g) 'V (lam () "Nope"))

(send (send a ref 'b)
   sync (send a ref 'f) (lam () "Can't be real!"))

(displayln "These two should be the same")
(send (send a ref 'b) repr)
(send (send a ref 'f) repr)

; Try modifying f from somewhere else
(def m2 (new mole%))
(send m2 sync (send a ref 'b) (lam () "Can't fail"))
(send m2 update '(h i) 'T (lam () "Failure not an option"))
(displayln "These three should be the same")
(send (send a ref 'b) repr)
(send m2 repr)
(send (send a ref 'f) repr)

; Try a fail update
(send m2 update '(h) 'R (lam () "Not fail yet"))
(check-equal? (send m2 update '(h) 'RR (lam () "Now we fail"))
              "Now we fail")

; Sync from the upper level and change on the lower level
(def m3 (new mole%))
(send m3 update '(tir) 'X (lam () "No"))
(send m3 update '(tal) 'UNKNOWN (lam () "No"))
(def m4 (new mole%))
(send m4 update '(eth) 'Y (lam () "No"))
(send m4 update '(el) 'Z (lam () "No"))
(send m4 update '(tir) 'UNKNOWN (lam () "No"))
(send m4 update '(tal) 'UNKNOWN (lam () "No"))
(send m3 sync m4 (lam () "Still No"))
(send (send m3 ref 'tal)
  update null 'S (lam () "Nope"))
(check-eq? (send (send m4 ref 'tal) get-data)
           'S)
