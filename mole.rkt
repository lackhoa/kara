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
    (init [children-i (seteq)])
    (init [synced-i (seteq)])

    (def data data-i)
    (def children children-i)
    ; Don't let this set contain ourselves
    (def synced synced-i)

    (define/public (get-data)
      data)
    (define/public (get-children)
      children)
    (define/public (get-synced)
      synced)

    (super-new)

    (define/public (ref path)
      (let ([m-lu
             (delay (assq (car path) data))])
        (cond [(null? path) data]
              [(force m-lu)
               (send m-lu ref (cdr path))]
              [else 'NOT-FOUND])))

    (def (expand path val)
      (if (null? path)
          (set! data val)
        (let ([new-child (new mole%)])
          (set! chilren
            (cons (cons (car path) new-child)
                  children))
          ; Recursively let the child do the work
          (send new-child
                expand (cdr path) val))))

    (define/public (update path
                           val
                           fail-con
                           [exclude (seteq)])
      (def m-lu
        (delay (assq (car path) data)))

      (if (null? path)
          (cond [(eq? data 'UNKNOWN)
                 (set! data val)]
                [(uneq? data val)
                 (fail-con)]
                [else (return 'NOT-CHANGED)])
        (if (force m-lu)
            ; There is a child of that role
            (send m-lu
                  update (cdr path)
                         val
                         fail-con
                         exclude)
          ; There is no child of that role yet
          (begin (expand path val)
                 (set! newborn-flag #t))))
      ; Inform others about the update,
      ; except for those in `exclude`
      (for-each
        (lam (subject)
          (send subject
                update path
                       val
                       fail-con
                       ; For them: exclude themselves
                       (set-union exclude synced)))
        (set-subtract synced exclude)))

    (define/public (sync-with m-other)
      (let/ec return
        (when (eq? m-other this)
          (return 'SELF-SYNC))

        (when (set-member? synced m-other)
          (return 'ALREADY-SYNCED))

        ; Start out with syncing data
        (def other-data
             (send m-other get-data))
        (cond
          ; No conflict
          [(eq? data other-data) (void)]
          ; `m-other` has new intel for us
          [(eq? data 'UNKNOWN)
           (update null
                   other-data
                   (lam () (return 'INCONSISTENT))
                   ; The other guy already knew this,
                   ; since it came from him.
                   (seteq m-other))]
          ; We have new intel for `m-other`
          [(eq? m-other UNKNOWN)
           (send m-other
                 update null
                        data
                        (lam () return 'INCONSISTENT)
                        ; Similarly, we already knew this
                        (seteq this))])

        ; Then we move on to syncing children
      (set-add! synced m-other)))))
