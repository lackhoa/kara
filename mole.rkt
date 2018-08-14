#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out)
         (all-from-out))

; ---------------------------------
; Molecules
; ---------------------------------
(def (make-new-mole) null)
(def (make-entry path val)
  (cons path val))

; Returns the index and the entry containing `path`
; Returns two values of NOT-FOUND if not found
(def (mole-lookup mole path)
  (let loop ([ls mole] [id 0])
    (if (null? ls)
        (values 'NOT-FOUND 'NOT-FOUND)
      (let ([e-focus (car ls)])
        (if (set-member? (car e-focus) path)
            (values id e-focus)
          (loop (cdr ls) (+ id 1)))))))

(def (mole-ref mole path)
  (let-values ([(_ entry) (mole-lookup mole path)])
    (if (eq? entry 'NOT-FOUND)
        'UNKNOWN
      (cdr entry))))

; Returns the tail when `prefix` is in `path`, otherwise #f
(def (prefix pre path)
  (let-values ([(x y)
                (drop-common-prefix pre path)])
    (if (null? x) y #f)))

(def (pad pre path)
  (append pre path))

(def (dab-all post st-key)
  (set-map st-key
           ((lam (item) (pad item post)))))

; Add an equality from `x` to `y` to `mole`
; Returns: the new molecule
(def (add-eq mole x y)
  (def (weed key)
    (let ([ls-key (set->list key)]
          [dx null] [dy null])
      (for-each
        (lam (item)
          (let ([px (prefix x first)]
                [py (delay (prefix y first))])
            (cond [px         (set! dx (cons px dx))]
                  [(force py) (set! dy (cons py dy))])))
        ls-key)
      (values dx dy)))

  (let/ec return  ; Escape point for inconsistencies
    (def new-mole (make-new-mole))
    (for-each
      ; We add in each entry from the `mole` to `new-mole`
      (lam (e-mole-itr)
        ; This function mutates `e-mole-itr`
        (def (equate pre other-pre ls-post)
          (for-each
            (lam (t)  ; `t` is the postfix
              (def kt (pad other-pre t))  ; `kt` is `xt` or `yt`
              (define-values (id e)
                (mole-lookup new-mole kt))
              (cond
                [(eq? e 'NOT-FOUND)
                 ; Add `kt` in
                 (set-add! (car e-mole-itr) kt)]

                ; Error if the available value is different
                [(unequal? (cdr e)
                           (cdr e-mole-itr))
                 (return 'INCONSISTENT)]

                ; Merge the entries if consistent
                [else
                 ; Delete the entry `e`
                 (set! new-mole
                       (remove-pos new-mole id))
                 ; Add all the keys from `e` to the key
                 (set-union! (car e-mole-itr) (car e))]))
            ls-post))

        (let* ([(dx dy) (weed key)])
          (equate x y dx)
          (equate y x dy)
          (set! new-mole
            (cons e-mole-itr new-mole))))
      mole)
    ; If x and y are not in `new-mole`, then we must state
    ; explicitly that they are equal
    (when (and (eq? (mole-ref new-mole x) 'UNKNOWN)
               (eq? (mole-ref new-mole y) 'UNKNOWN))
        (set! new-mole
          (cons (cons (set x y) 'UNKNOWN)
                new-mole)))
    new-mole))

; Return all splits of a path, starting with
; `(path, null)` and NOT ending with `(null, path)`
(def (all-splits path)
  (let loop ([accum (list (cons path null))]
             [pre null] [post path])
    (if (null? post)
        accum
      (let ([new-pre (append1 pre (car post))]
            [new-post (cdr post)]
            [pair (cons pre post)])
        (loop (cons pair accum)
              new-pre
              new-post)))))

(def (mole-update mole path value)
  (let/ec return
    (for-each
      (lam (pre-post)
        (def focus (car pre-post))
        (def post (cdr pre-post))
       (let-values ([(_ e)
                     (mole-lookup mole focus)])
         (unless (eq? e 'NOT-FOUND)
           (if (null? post)
               ; The focus is `path`
               (when (unequal? (cdr e) value)
                 (return 'INCONSISTENT))
             (return (cons (make-entry (dab-all (car e))
                                       (cdr e))
                           mole))))))
      (all-splits path))

    ; If the code ever gets here, then `path` has
    ; no linked ancestor, including itself.
    (cons (make-entry (set path) value)
          mole)))
