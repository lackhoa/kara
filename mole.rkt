#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out)
         (all-from-out))

; ---------------------------------
; Molecules
; ---------------------------------
(def mole%
  (class object%
    (init [data-i 'UNKNOWN])
    (init [children-i null])
    (init [linked-i null])

    (def data data-i)
    (def children children-i)
    (def linked linked-i)

    (super-new)

    (define/public (ref path)
      (let ([m-lu
             (delay (assq (car path) data))])
        (cond [(null? path) data]
              [(force m-lu)
               (send m-lu ref (cdr path))]
              [else 'NOT-FOUND])))

    (define/public (update path
                           value
                           [exclude null]
                           fail-con)
      (def m-lu
        (delay (assq (car path) data)))
      (if (null? path)
          (cond [(eq? data 'UNKNOWN)
                 (set! data value)]
                [(uneq? data value)
                 (fail-con)])
        (if (force m-lu)
            (send m-lu
                  update (cdr path)
                         value
                         exclude
                         fail-con)
          (let ([new-child (new mole%)])
            (set! children
                  (cons new-child children))
            (send new-child
                  update (cdr path)
                         value
                         exclude
                         fail-con))))
      ; Then inform others (except for those in `exclude`)
      (for-each
        (lam (m-linked)
          (send m-linked
                update path
                       value
                       (set-union exclude linked)
                       fail-con))
        (set-subtract (set-add linked this)
                      exclude)))

    (define/public (add-link m-other)
      (set-add! linked m-other))))
