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

        ['members (hash-keys dic)]

        ['copy (make-mole (hash-copy dic)
                          (hash-copy slinks))]

        ['add-slink
         (lam (from to) (hash-set! slinks from to))]
        
        ['ref
         (lam (path)
           (if (sym-link? path)
               (hash-ref dic (follow-slink path))
             (hash-ref dic path)))]

        ; Returns false if there is an inconsistency
        ['update
         (lam (path val)
           (let ([my-path path])
             (when (sym-link? path)
               (set! my-path
                     (follow-slink path)))
             (if (hash-contain? dic my-path)
                 (unless (equal? (hash-ref dic my-path))
                         #f)
               (hash-set! dic my-path val))))]

        [else (error "MOLECULE" "Unknown message" msg)]))
    me))

; This is meant for updating according to a model
; with relative paths
(def (pad path rel)
    (if (non-empty-string? path)
        (string-append path "/" rel)
      path))
