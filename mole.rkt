#lang racket
(require "lang/kara.rkt")
(provide mole%)

(def mole%
  (class object%
    (init [new-content (make-hash)])

    (def content new-content)

    (super-new)

    (define/public (mole-update path value)
      (when (null? path)
        (error "mole-ref" "Empty path encountered" mole)))

      (def (loop mole ls-path)
        (def first (car ls-path))
        (def rest (cdr ls-path))
        (if (hash-has-key? content first)
            (if (null? rest)
                (let ([clone (mole-clone this)])
                  (send clone set-content! value))
                (loop (hash-ref first) rest))

            (if (null? rest)
                ())))

    (define/public (mole-ref path)
      (when (null? path)
        (error "mole-ref" "Empty path encountered" mole))

      (def (loop mole ls-path)
        (def first (car ls-path))
        (def rest (cdr ls-path))
        (if (hash-has-key? content first)
            (if (null? rest)
                (hash-ref first)
                (loop (hash-ref first) rest))
            #f))

      (loop this (string-split path "/")))))

