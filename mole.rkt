#lang racket
(require "lang/kara.rkt"
         "atom.rkt")
(provide (all-defined-out)
         (all-from-out "atom.rkt"))

; ---------------------------------
; Molecules
; ---------------------------------
; Molecules are just hash tables containing atoms.
; Also, they can have symbolic links.

(def (mole? exp) (tagged? exp 'Mole))

(def (new-mole)
  (make-mole (make-hash) (make-hash)))

(def (make-mole ht slinks)
  (let ([dic ht] [slinks slinks])
    (def (me msg)
      (switch msg
        ['dic dic]
        ['slinks slinks]

        ['members (hash-keys dic)]

        ['copy (make-mole (hash-copy dic)
                          (hash-copy slinks))]

        ['sym-link?
         (lam (path) (hash-has-key? slinks path))]

        ['add-slink
         (lam (from to) (hash-set! slinks from to))]

        ['follow-slink
         (lam (link) (hash-ref slinks link))]

        ; Reference an atom, given that it's in the dictionary
        ['ref
         (lam (path)
           (if ((me 'sym-link?) path)
               (hash-ref dic
                         ((me 'follow-slink) path))
             (hash-ref dic path)))]

        ['update
         (lam (path val)
           (let ([my-path path])
             (when ((me 'sym-link?) path)
               (set! my-path
                     ((me 'follow-slink) path)))
             (if (hash-has-key? dic my-path)
                 (hash-set! dic
                   my-path
                   (atom-intersect ((me 'ref) my-path)
                                   val))
               (hash-set! dic my-path val))))]

        [else (error "MOLECULE" "Unknown message" msg)]))
    me))

; This is meant for updating according to a model
; with relative paths
(def (pad path rel)
    (if (non-empty-string? path)
        (string-append path "/" rel)
      path))
