#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         rackunit)

(def m1
  (new mole% [data-i 'gaylord]))

(check-eq? (send m1 get-data)
           'gaylord)

(send m1 expand '(d) 'w)

(def d (send m1 ref 'd))

(check-eq? (send d get-data) 'w)

(send m1
  update '(a)
         'x
         (lam () "How the heck did I fail?"))

(check-equal? (send m1 get-roles)
              '(a d))

(def a (send m1 ref 'a))

(check-equal? (send a get-data) 'x)

(check-equal? (send a get-sync-ls)
              (list a))

(send m1
  update '(a-clone)
         'UNKNOWN
         (lam () "That can't fail!"))

(def a-clone (send m1 ref 'a-clone))

(send a-clone
  sync a
       (lam () "Won't fail, for sure"))
