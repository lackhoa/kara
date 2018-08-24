#lang racket

(require "macro.rkt"
         "list-prims.rkt")
(provide (all-defined-out)
         (all-from-out "list-prims.rkt"))

; This file focuses on the lazy way of working with sequences
; The terminology is like this:
; Stream = Lazy Sequence
; List = Strict Sequence

; Test if `x` is in `seq`, but returns the tail when hit
(def (stream-member x seq)
  (cond [(stream-empty? seq)
         #f]

        [(equal? x (car seq))
         seq]

        [else
         (stream-member x (cdr seq))]))

(def (exists pred seq)
  (cond [(stream-empty? seq) #f]
        [(pred (stream-first seq)) #t]
        [else
         (exists pred (stream-rest seq))]))

(def (forall pred seq)
  (exists (negate pred) seq))

; Mapping and reducing with append to create nested maps
(def (stream-flatmap proc seq)
  (stream-fold stream-append
               empty-stream
               (stream-map proc seq)))

(def (stream-remove x s)
  (stream-filter (lam (item)
                   (not (equal? item x)))
                 s))

; `ls`: a list of streams.
(def (stream-interleave ls)
  (match ls
    ['() empty-stream]
    [(list s) s]
    [ls
     (let ([first (car ls)]
           [rest  (cdr ls)])
       (if (stream-empty? first)
           (stream-interleave (cdr ls))
         (stream-cons (stream-first first)
                      (stream-interleave (append1 rest
                                                  (stream-rest first))))))]))

(def (remove-pos ls pos)
  (cond [(= pos 0) (drop ls 1)]
        [(> pos 0) (append (take ls pos)
                           (drop ls (+ pos 1)))]
        [else (error "REMOVE-POS" "Invalid position" pos)]))

; Add an item to the end of a list
(def (pad ls single-item)
  (append ls (list single-item)))

; Synonym
(def append1 pad)

;; (def (permutations s)
;;   ; This function sticks the x to the permutations that doesn't contain x
;;   (def (permute-aux x)
;;     (map (lam (p) (cons x p))
;;          (permutations (remove x s))))

;;   (if (null? s)
;;       (list null)  ; Sequence containing empty set
;;     (flatmap permute-aux s)))

;; ; params: seq (a sequence of sequences to take product).
;; ; returns: a sequence of lists as products
;; ; This function is lazy
;; (def (product seqs)
;;     ; The algorithm of the foldr
;;     (def (prod-aux first prod-rest)
;;         (flatmap (lam (iter-first)
;;                     (map (lam (iter-prod-rest)
;;                            (cons iter-first iter-prod-rest))
;;                          prod-rest))
;;                  first))

;;     (lfoldr prod-aux
;;              (list null)  ; Base case: kind of undefined?
;;              seqs))

;; ; params s: a single sequence as a set
;; ;           (although not guaranteed by the algorithm).
;; ; returns: a sequence of list as subsets of the given set
;; ; Still lazy
;; (def (powerset s)
;;     (def (pow-aux first pow-rest)
;;       (append (map (lam (pow-rest-iter)
;;                      (cons first pow-rest-iter))
;;                    pow-rest)                         ; Include first
;;               pow-rest))                             ; Exclude first

;;     (lfoldr pow-aux
;;             (list null)  ; Base case: the only subset of the empty set is itself
;;             s))
