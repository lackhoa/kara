#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         rackunit)

; The first hash table indicates the form,
; The second hash table indicates the symbolic links.
(def B1
  (list
    (make-hash
      '(["" . 'eq]
        ["lhs" . 'disjunction]
        ["rhs" . 'disjunction]))
    (make-hash
      '(["lhs/fd" . "rhs/sd"]
        ["lhs/sd" . "rhs/fd"]))))

(def B2
  (list
    (make-hash
      '(["" . 'eq]
        ["lhs" . 'disjunction]
        ["rhs" . 'disjunction]
        ["lhs/sd" . 'disjunction]
        ["rhs/fd" . 'disjunction]))
    (make-hash
      '(["lhs/fd" . "rhs/fd/fd"]
        ["lhs/sd/fd" . "rhs/fd/sd"]
        ["lhs/sd/sd"] . "rhs/sd"))))
