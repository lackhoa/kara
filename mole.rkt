#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out)
         (all-from-out))

; ---------------------------------
; Molecules
; ---------------------------------
; Molecules are just hash tables containing symbolic values.
; Also, they can have symbolic links.

(def (new-mole)
  (make-mole (make-hash) (make-hash)))

(def (make-mole ht slinks)
  (let ([dic ht] [slinks slinks])

    (def (follow-slink sym-link)
      (hash-ref slinks sym-link))

    (def (sym-link? path)
      (hash-has-key? slinks path))

    (def (me msg)
      (switch msg
        ['dic dic]
        ['slinks slinks]

        ['link?
         (lam (path)
           (hash-has-key? slinks path))]

        ['members (hash-keys dic)]

        ['copy (make-mole (hash-copy dic)
                          (hash-copy slinks))]

        ; Returns INCONSISTENT if the values are already different
        ['add-slink
         (lam (path1 path2)
           (let ([i1 ((me 'ref) path1)]
                 [i2 ((me 'ref) path2)])
             (cond [(and (eq? i1 'NOT-FOUND)
                         (eq? i2 'NOT-FOUND))
                    (hash-set! slinks path1 path2)]

                   ; At least one of them are found
                   [(eq? i1 'NOT-FOUND)
                    (hash-set! slinks path1 path2)]
                   [(eq? i2 'NOT-FOUND)
                    (hash-set! slinks path2 path1)]

                   ; Don't do anything if both values already the same
                   [else
                    (unless (equal? i1 i2) 'NOT-FOUND)])))]

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
