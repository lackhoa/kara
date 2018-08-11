#lang racket

(require "macro.rkt"
         "list_prims.rkt")
(provide (all-defined-out)
         (all-from-out "list_prims.rkt"))

; This file focuses on the lazy way of working with sequences
; My representation of streams is by delaying and forcing,
; not by using a special form. Therefore, `car` and `cdr` is different.
; I also reuse `stream`, because the representation is different.
; The terminology is like this:
; Stream = Lazy Sequence
; List = Strict Sequence

; -----------------------------------------------
; Foundation of the Stream Paradigm
; -----------------------------------------------

; Remember that when you use `cons` with a delayed list, you're
; actually creating an improper list.

; Forces the first value, if it's a promise.
(def (car seq)
  (force (lcar seq)))

; Forces the rest of the sequence, if it's a promise.
(def (cdr seq)
  (force (cdr seq)))

; A useful short-hand.
(def (cadr seq)
  (car (cdr seq)))

; Note that the null stream must be exactly the empty list

; Useful functions
; -----------------------------------------------

; Test if `x` is in `seq`, but returns the tail when hit
(def (member x seq)
  (cond [(null? seq) #f]
        [(equal? x (car seq)) seq]
        [else (member x (cdr seq))]))

; The default, lazy version of foldr.
(def (foldr op init seq)
    (if (null? seq)
        init
        (op (delay (car seq))
            (delay (foldr op init (cdr seq))))))

; This is a strict version of `foldr`.
; Same thing as `foldr`, but the arguments are forced before `op`.
(def (lfoldr op init seq)
  (foldr (lam (x y)
            (op (force x) (force y)))
         init
         seq))

(def (filter pred seq)
  (foldr (lam (x y)
           (if (pred (force x))
               (cons x y)
             (force y)))
          null
          seq))

; the use of `foldr` is safe because `y` won't be forced if `x` satisfies
(def (exists pred seq)
  (foldr (lam (x y)
            (if (pred (force x))
                #t
              (exists pred (force y))))
         #f
         seq))

(def (forall pred seq)
  (foldr (lam (x y)
            (if (pred (force x))
                (forall pred (force y))
              #f))
         #t
         seq))

(def (append seq1 seq2)
  (foldr cons seq2 seq1))

; Mapping and reducing with append to create nested maps
(def (flatmap proc seq)
  (lfoldr append null (map proc seq)))

(def (remove x s)
  (filter (lam (item)
            (not (equal? item x)))
          s))

(def (permutations s)
  ; This function sticks the x to the permutations that doesn't contain x
  (def (permute-aux x)
    (map (lam (p) (cons x p))
         (permutations (remove x s))))

  (if (null? s)
      (list null)  ; Sequence containing empty set
    (flatmap permute-aux s)))

(def (seq->list seq)
  (lfoldr cons null seq))

(def (length sequence)
  (lfoldr (lam (x y) (+ 1 y))
          0
          sequence))

; Uppser can be null, in which case the stream is infinite.
(def (range lower upper)
  (if (null? upper)
      (cons lower
            (delay (range (+ 1 lower) upper)))
      (if (>= lower upper)
          null
          (cons lower
                (delay (range (+ 1 lower) upper))))))

; Since foldr is lazy, map is lazy
(def (map func L)
  (foldr (lam (x y)
           (cons (delay (func (force x)))
                 y))
         null
         L))

(def (wrap thing)
  (list thing))

(def (interleave s1 s2)
  (if (null? s1)
      s2
      (cons (delay (car s1))
            (delay (interleave s2 (cdr s1))))))

; params: seq (a sequence of sequences to take product).
; returns: a sequence of lists as products
; This function is lazy
(def (product seqs)
    ; The algorithm of the foldr
    (def (prod-aux first prod-rest)
        (flatmap (lam (iter-first)
                    (map (lam (iter-prod-rest)
                           (cons iter-first iter-prod-rest))
                         prod-rest))
                 first))

    (lfoldr prod-aux
             (list null)  ; Base case: kind of undefined?
             seqs))

; params s: a single sequence as a set
;           (although not guaranteed by the algorithm).
; returns: a sequence of list as subsets of the given set
; Still lazy
(def (powerset s)
    (def (pow-aux first pow-rest)
      (append (map (lam (pow-rest-iter)
                     (cons first pow-rest-iter))
                   pow-rest)                         ; Include first
              pow-rest))                             ; Exclude first

    (lfoldr pow-aux
            (list null)  ; Base case: the only subset of the empty set is itself
            s))
