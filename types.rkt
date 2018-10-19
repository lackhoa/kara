#lang racket
(require "lang/kara.rkt"
         "mol.rkt")
(provide (all-defined-out))

;;; Models
(def (get-model x)
  (match x
    ['->           `(mol% -> (var% 0) (var% 1))]
    [(? symbol?)  `(mol% ,x)]
    [(? number?)  `(mol% #f ,@(for/list ([i  (range x)])
                                `(var% ,i)))]))

;;; Axioms
(def i '(mol% -> (var% 0) (var% 0)) )

(def k '(mol% ->
              (var% 0)
              (mol% -> (var% 1) (var% 0))))

(def p '(mol% =>
              (mol% -> (var% 0) (var% 1))
              (var% 0)
              (var% 1)))

(def s
  ;; (-> (-> A
  ;;         (-> B C))
  ;;     (-> (-> A B)
  ;;         (-> A C)))
  '(mol% ->
         (mol% ->
               (var% 0)
               (mol% -> (var% 1) (var% 2)))
         (mol% ->
               (mol% ->
                     (var% 0)
                     (var% 1))
               (mol% ->
                     (var% 0)
                     (var% 2)))))

(def ak
  '(mol% => (var% 0) (var% 1) ,k))

(def as
  '(mol% => (var% 0) (var% 1) ,s))

(def mp '(mol% =>
               (mol% =>
                     (var% 2)
                     (var% 3)
                     (mol% -> (var% 0) (var% 1)))
               (mol% => (var% 4) (var% 5) (var% 0))
               (mol% => (var% 4) (var% 5) (var% 1))))

(def axioms `(,ak ,as ,mp))
(def maxims `(,k ,s ,p))

;;; Some theorems to prove
(def w
  ;; (A -> (A -> B)) -> (A -> B)
  '(mol% ->
         (mol% ->
               (var% 0)
               (mol% -> (var% 0) (var% 1)))
         (mol% -> (var% 0) (var% 1))))

(def c
  ;; (A -> (B -> C)) -> (B -> (A -> C))
  '(mol% ->
         (mol% -> (var% 0) (mol% ->
                                (var% 1)
                                (var% 2)))
         (mol% -> (var% 1) (mol% ->
                                (var% 0)
                                (var% 2)))))

(def b
  ;; (B -> C) -> ((A -> B) -> (A -> C))
  '(mol% ->
         (mol% -> (var% 1) (var% 2))
         (mol% ->
               (mol% -> (var% 0) (var% 1))
               (mol% -> (var% 0) (var% 2)))))
