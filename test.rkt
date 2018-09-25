#lang racket
(require "lang/kara.rkt"
         "mole.rkt")

(def (parse mol)
  (match (mol%-data mol)
    ['mp=>  (list (parse (list-ref (mol%-kids mol) 1))
                  (parse (list-ref (mol%-kids mol) 2)))]
    ['ai=>  'i]
    ['ak=>  'k]
    ['as=>  's]))

(call-with-input-file "db/data.rkt"
  (lam (in)
    (for ([m  (read in)])
      (displayln (parse m))
      (newline))))
