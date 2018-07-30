#lang racket
(require "lang/kara.rkt")
(provide mole%)

(def mole%
  (class object%
    (init [new-content (make-hash)])

    (def content new-content)

    (super-new)

    (define/public (mole-update path value)
      (is-a? ))

    (define/public (mole-ref path)
      (if (hash-has-key? content path)
        ; Will return a completely empty molecule if the path
        ; is found nowhere, but that's fine.
        (hash-ref content path)
        (let clone (make-hash)
          (for-each (lam (p)
                      (hash-set! clone p (hash-ref content path)))
                    (filter (lam (s) (string-prefix? path s))
                            (hash-keys content)))
          clone)))))

