#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
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
    [(list x y)  (>> (pull mp '[1] (construct x))
                     (lam (m)
                       (pull m '[2] (construct y))))]
    [x           (match x
                   ['i  ai]
                   ['k  ak]
                   ['s  as]
                   [_   (update new-root '[] '=>)
                        #|An anonymous assertion|#])]))

(def (type e)
  (dm (construct e)))

(dm (construct '(k (k k))))
