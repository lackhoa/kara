#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt")

(def (parse mol)
  (match (mol%-data mol)
    ['mp=>  (list (parse (list-ref (mol%-kids mol) 1))
                  (parse (list-ref (mol%-kids mol) 2)))]
    ['ai=>  'i]
    ['ak=>  'k]
    ['as=>  's]))

(def (construct e)
  ;; e is a combinatory expression like ((i s) k)
  (match e
    [(list x y)  (match (pull mp
                              '[1]
                              (construct x))
                   [#f  #f]
                   [m   (pull m
                              '[2]
                              (construct y))])]
    [x           (match x
                   ['i  ai]
                   ['k  ak]
                   ['s  as]
                   [_   (mol% '=> null)
                        #|An anonymous assertion|#])]))

(def (type e)
  (dm (construct e)))

'((s i) i)
'(((s i) i) x)
