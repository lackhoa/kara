#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out)
         (all-from-out))

; ---------------------------------
; Molecules
; ---------------------------------
(def (make-new-mole) null)

(def (mole-lookup mole path)
  (let loop ([ls mole])
    (if (null? ls)
        'NOT-FOUND
      (let ([focus (car ls)])
        (if (set-member? focus path)
            path
          (loop (cdr mole)))))))

(def (mole-ref mole path)
  (let ([lookup (mole-lookup mole path)])
    (if (eq? lookup 'NOT-FOUND)
        'UNKNOWN
      lookup)))

; Returns the tail when `prefix` is in `path`, otherwise #f
(def (prefix pre path)
  (let-values ([(x y)
                (drop-common-prefix pre path)])
    (if (null? x) y #f)))

(def (pad pre path)
  (append pre path))

; Add an equality from `x` to `y` in `mole`
(def (add-eq mole x y)
  (def (weed key)
    (let ([ls-key (set->list key)])
      (let loop ([ls ls-key] [dx null] [dy null])
        (if (null? ls)
            (values dx dy)
          (let ([px (prefix x first)])
            (if px
                (loop (cdr ls)
                      (cons px dx)
                      dy)
              (let ([py (prefix y first)])
                (if py
                    (loop (cdr ls)
                          dx
                          (cons py dy))
                  (loop (cdr ls)
                        dx
                        dy)))))))))

  (let loop ([cur-mole (make-new-mole)]
             [ls mole])
    (if (null? ls)
        mole
      (let*-values ([(new-mole cur-mole)]
                    [(e-focus) (car ls)]
                    [(dx dy) (weed (car e-focus))])
        (def (equate pre ls)
          (let loop ([ls ls])
            (unless (null? ls)
              (let* ([t (car ls)]
                     [kt (pad pre t)])
                (let ([lookup (mole-lookup new-mole kt)])
                  (if (eq? lookup 'NOT-FOUND)
                      (set! new-mole
                            (cons (add-to-key (car new-mole)
                                              kt)
                                  (cdr new-mole)))
                    (if (not (equal? (cdr lookup)
                                     (mole-ref new-mole kt)))
                        (Yeah I don't know what the fuck to do...)
                      (begin)))
                  (loop (cdr ls)))))))
        (equate dx x)
        (equate dy y)
        (loop new-mole (cdr ls))))))

(def (make-mole ht slinks)
  (let ([dic ht] [slinks slinks])
    (def (me msg)
      (switch msg

        ; Returns INCONSISTENT if the values are already different
        

        ; Returns NOT-FOUND for unknowns
        ['ref
         (lam (path)
           (if (sym-link? path)
               (hash-ref dic (follow-slink path) 'NOT-FOUND)
             (hash-ref dic path 'NOT-FOUND)))]

        ; Returns INCONSISTENT if there is an inconsistency
        ['update
         (lam (path val)
           (let ([my-path path])
             (when (sym-link? path)
               (set! my-path
                     (follow-slink path)))
             (if (hash-has-key? dic my-path)
                 (unless (equal? (hash-ref dic my-path))
                         'INCONSISTENT)
               (hash-set! dic my-path val))))]

        [else (error "MOLECULE" "Unknown message" msg)]))
    me))

; This is meant for updating according to a model
; with relative paths
(def (pad path rel)
    (if (non-empty-string? path)
        (string-append path "/" rel)
      path))
