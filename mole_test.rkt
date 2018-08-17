#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         rackunit)

(def m1
  (new mole% [data-i 'gaylord]))

(check-eq? (send m1 get-data)
           'gaylord)

(send m1 update-path '(d) 'W (lam () "ain't gonna fail"))

(def d (send m1 ref 'd))


(check-eq? (send d get-data) 'W)

(send m1
  update-path '(a) 'X (lam () "How the heck did I fail?"))

(check-equal? (send m1 get-roles)
              '(a d))

(def a (send m1 ref 'a))

(check-equal? (send a get-data) 'X)

(check-equal? (send a get-sync-ls)
              (list a))

(send m1
  update-path '(a-clone)
         'UNKNOWN
         (lam () "That can't fail!"))

(def a-clone (send m1 ref 'a-clone))

(send a-clone
  sync a (lam () "Won't fail, for sure"))

; Now we update-path a
(send a
  update-path '(ad) 'X (lam () "Won't fail"))

(def a-ad (send a ref 'ad))

(check-eq? (send a-ad get-data)
           'X)

; See what happens to a-clone
(def a-clone-ad (send a-clone ref 'ad))
(check-eq? (send a-clone-ad get-data)
           'X)

; adnother harder example:
(send a update-path '(b) 'Y (lam () "Impossible"))

(send a update-path '(b d) 'W (lam () "Definitely"))

(send a update-path '(c) 'Z (lam () "No fail"))

(send a update-path '(c e) 'V (lam () "Nope"))

;; (check-equal?
 ;; (send (send a ref 'b)
   ;; sync (send a ref 'c) (lam () "Will fail!"))
 ;; "Will fail!")

; Let's go again with the sync
(send a update-path '(f) 'Y (lam () "No fail"))

(send a update-path '(f g) 'V (lam () "Nope"))

(send (send a ref 'b)
   sync (send a ref 'f) (lam () "Can't be real!"))

;; (displayln "These two should be the same")
;; (send (send a ref 'b) repr)
;; (send (send a ref 'f) repr)

; Try modifying f from somewhere else
(def m2 (new mole%))
(send m2 sync (send a ref 'b) (lam () "Can't fail"))
(send m2 update-path '(h i) 'T (lam () "Failure not an option"))

; Here is where it fails, WTF, right?
(send (send m1 copy) repr)

;; (displayln "These three should be the same")
;; (send (send a ref 'b) repr)
;; (send m2 repr)
;; (send (send a ref 'f) repr)

; Try a fail update-path
;; (send m2 update-path '(h) 'R (lam () "Not fail yet"))
;; (check-equal? (send m2 update-path '(h) 'RR (lam () "Now we fail"))
              ;; "Now we fail")

; Sync from the upper level and change on the lower level
(def m3 (new mole%))
(send m3 update-path '(tir) 'X (lam () "No"))
(send m3 update-path '(tal) 'UNKNOWN (lam () "No"))
(def m4 (new mole%))
(send m4 update-path '(eth) 'Y (lam () "No"))
(send m4 update-path '(el) 'Z (lam () "No"))
(send m4 update-path '(tir) 'UNKNOWN (lam () "No"))
(send m4 update-path '(tal) 'UNKNOWN (lam () "No"))
(send m3 sync m4 (lam () "Still No"))
(send (send m3 ref 'tal)
  update-path null 'S (lam () "Nope"))
(check-eq? (send (send m4 ref 'tal) get-data)
           'S)

; Cloning
;; (pretty-print (send m1 repr))
;; (def m5 (send m1 copy))
;; (send m5 repr)

;; (def test (new mole%))
;; (def cp (send test copy))
;; (check-equal? (send cp get-sync-ls) (list cp))
;; (send cp update 'L (lam () "Won't fail"))
;; (check-eq? (send cp get-data) 'L)
;; (check-eq? (send test get-data) 'UNKNOWN)
;; (send cp update-path '(j) 'K (lam () 'NotGonnaHappen))
;; (def cpcp (send cp copy))
;; (check-eq? (send (send cpcp ref 'j) get-data)
;;            'K)
;; (send cp update-path '(k) 'K (lam () 'NotGonnaHappen))
;; ; Now let's try the syncing
;; (send (send cp ref 'j)
;;   sync (send cp ref 'k) (lam x x))
;; (def cnop (send cp copy))
;; (send cnop update-path '(j i) 'R (lam () "Damn"))
;; (check-eq? (send (send cnop ref '(j i)) get-data)
;;            (send (send cnop ref '(k i)) get-data))
