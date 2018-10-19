#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "types.rkt"
         "enum.rkt"
         rackunit)

(define-syntax-rule (up! iden more ...)
  (set! iden (up iden more ...)))

(test-case
 "Trivial"
 (check-false (instance? (up new-var '[] '(mol% a))
                         (up new-var '[] '(mol% b))))
 (check-true (instance? new-var new-var))
 )

(test-case
 "Generality"
 (check-false (instance? new-var
                         (get-model '->)))
 (check-true (instance? (get-model '->)
                        (get-model '->)))

 (check-true (instance? (msync (up (up (get-model 2) '[0] (get-model '->))
                                   '[1] (get-model '->))
                               '[0] '[1])
                        (msync (get-model 2) '[0] '[1])))
 (let ([base '(mol% #f (var% 0) (var% 0) (var% 1) (var% 1))])
   (check-false (instance? base
                           (msync base '[1] '[2])))
   (check-true (instance? (msync base '[1] '[2])
                          base)))
 )

(test-case
 "Multi-leveled"
 (def r
   ;; (-> (-> (-> A B) (-> A B))
   ;;     (-> (-> A B) (-> A B)))
   '(mol% ->
          (mol% -> (-> (var% 0) (var% 1)) (-> (var% 0) (var% 1)))
          (mol% -> (-> (var% 0) (var% 1)) (-> (var% 0) (var% 1)))))

 (check-true  (instance? r i))
 (check-false (instance? i r)))

(test-case
 "Generality Ultimate"
 (def r
   ;; (-> (-> A B) (-> C (-> A B)))
   '(mol% ->
          (mol% -> (var% 0) (var% 1))
          (mol% -> (var% 2) (mol% -> (var% 0) (var% 1)))))

 (check-true (instance? r k))
 (check-false (instance? k r))
 (check-true (< (size k)
                (size r))))

(test-case
 "Super edgy cases (well not really)"
 (def r1
   ;; (-> (-> A B) (-> B A))
   '(mol% ->
          (mol% -> (var% 0) (var% 1))
          (mol% -> (var% 1) (var% 0))))

 (def r2
   ;; (-> (-> A B) (-> A B))
   '(mol% -> (mol% -> (var% 0) (var% 1)) (mol% -> (var% 0) (var% 1))))

 (check-false (instance? r2 r1))

 (def r3
   ;; (-> (-> A ?) (-> ? A))
   '(mol% -> (mol% -> (var% 0) (var% 1)) (mol% -> (var% 2) (var% 0))))

 (check-false (instance? r2 r3))
 (check-true (instance? r1 r3))
 )
