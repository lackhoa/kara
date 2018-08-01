#lang racket

(require "macro.rkt"
         "seq.rkt"
         "utils.rkt"
         rackunit)

(check-equal? #t (in-seq? 3 (list 1 2 3)) "in-seq 1")

(check-equal? #f (in-seq? 4 (list 1 2 3)) "in-seq2")

(def new-range (range 5 10))
(check-equal? 9 (seq-ref new-range 4) "Range and seq-ref")

(check-equal? 120 (lreduce * 1 (list 1 2 3 4 5)) "Strict Reduce")

(check-equal? 81 (seq-ref (map square new-range) 4) "Mapping")

(def (prod-of-squares-of-odds seq)
    (lreduce *
             1
             (map square
                  (filter odd? seq))))
(check-equal? 225 (prod-of-squares-of-odds (list 1 2 3 4 5)) "Reduce + map + filter")

(check-equal? 6 (length (permutations (list 1 2 3))) "Permutation")

(check-equal? 7 (length (append (range 2 6) (range 11 14))) "Append")

(def ingredient (list (range 0 3) (range 4 6) (range 7 10)))
(check-equal? 18 (length (product ingredient)) "Product of three sequences")

(check-equal? 16 (length (powerset '(A B C D))) "Powerset of {A, B, C, D}")

(def (scale factor seq)
    (map (lam (x) (* x factor)) seq))
(check-equal? '(1 11 2 22 3 33)
              (seq->list (interleave (range 1 4) (scale 11 (range 1 4))))
               "Interleaving")

(def (divisible? x y) (= (remainder x y) 0))
(def (sieve stream)
  (cons
    (car stream)
    (delay (sieve (filter (lam (x)
                             (not (divisible? x (car stream))))
                          (cdr stream))))))

(def primes (sieve (range 2 null)))
(check-equal? 233 (seq-ref primes 50) "Sieve of Eratosthenes")
