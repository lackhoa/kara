#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         rackunit)


(def refl (new-mole))
((refl 'add-slink) "conse/elem1" "ante/mem")
((refl 'add-slink) "conse/elem2" "ante/mem")
((refl 'add-slink) "conse/set"   "ante/set")

(def (mex only)
  (make-explicit (set only)))

((refl 'update) "ante/mem"  (mex 'a))
((refl 'update) "conse/set" (mex 'A))

(check-equal? ((refl 'ref) "conse/elem1") (mex 'a))
(check-equal? ((refl 'ref) "ante/set")    (mex 'A))
