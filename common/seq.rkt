#lang racket

(require "kara.rkt" "list_prims.rkt")
(provide car cdr cadr seq-ref for-each in-seq? map
         reduce strict-reduce product interleave length
         powerset permutations flatmap seq->list range
         append filter)

; This file focuses on the lazy way of working with sequences but all
; functions are suitable for strict sequences (many are as efficient).
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
    (force (lcdr seq)))

; A useful short-hand.
(def (cadr seq)
    (car (cdr seq)))


; The empty stream is the same as the empty list.
; It is a unique value, not even the delayed empty list is equal
(def empty-stream '())

; list-ref must be re-design since the one in Racket
; is random-access
(def (seq-ref seq index)
    (cond [(null? seq) (error "seq-ref" "Out of bound" (list seq index))]
          [(= index 0) (car seq)]
          [else (seq-ref (cdr seq)
                         (- index 1))]))

; Like map but for side-effects
(def (for-each proc s)
    (if (null? s)
        'done
        (begin (proc (car s))
               (for-each proc (cdr s)))))

; Useful functions
; -----------------------------------------------
(def (in-seq? x seq)
     (cond [(null? seq) #f]
           [(equal? x (car seq)) #t]
           [else (in-seq? x (cdr seq))]))

; This is a strict function
(def (strict-reduce op init seq)
    (if (null? seq)
        init
        (op (car seq)
            (strict-reduce op init (cdr seq)))))

; The lazy version
(def (reduce op init seq)
    (if (null? seq)
        init
        (op (car seq)
            (delay (reduce op init (cdr seq))))))

; Notice the force on `y` (since y was lazy)
(def (filter pred seq)
  (reduce (lam (x y)
                 (if (pred x) (cons x y) (force y)))
               null
               seq))

(def (append seq1 seq2)
   (reduce cons seq2 seq1))

; Mapping and reducing with append to create nested maps
(def (flatmap proc seq)
    (strict-reduce append null (map proc seq)))

(def (remove x s)
    (filter (lam (item) (not (eq? item x)))
            s))

(def (permutations s)
    ; This function sticks the x to the permutations that doesn't contain x
    (def (permute-aux x)
        (map (lam (p) (cons x p))
             (permutations (remove x s))))

    ; Main job done here
    (if (null? s)
        (list null)  ; Sequence containing empty set
        (flatmap permute-aux s)))

(def (seq->list seq)
    (strict-reduce cons null seq))

(def (length sequence)
    (strict-reduce (lam (x y) (+ 1 y)) 0 sequence))

; Uppser can be null, in which case the stream is infinite.
(def (range lower upper)
  (if (null? upper)
      (cons lower
            (delay (range (+ 1 lower) upper)))
      (if (>= lower upper)
          null
          (cons lower
                (delay (range (+ 1 lower) upper))))))

(def (map func L)
  (reduce (lam (x y) (cons (func x) y))
          null
          L))

(def (wrap thing)
    (list thing))

; params: seq (a sequence of sequences to take product).
; returns: a sequence of lists as products
; This function is lazy
(def (product seqs)
    ; The algorithm of the reduce
    (def (prod-aux first prod-rest)
        (flatmap (lam (iter-first)
                    (map (lam (iter-prod-rest)
                           (cons iter-first iter-prod-rest))
                         prod-rest))
                 first))

    (strict-reduce prod-aux
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

    (strict-reduce pow-aux
                   (list null)  ; Base case: the only subset of the empty set is itself
                   s))

(def (interleave s1 s2)
  (if (null? s1)
      s2
      (cons (car s1)
            (delay (interleave s2 (cdr s1))))))
