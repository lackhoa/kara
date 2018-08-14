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
    (init [synced-i null])

    (def data data-i)
    (def children children-i)
    ; The sync list is always synced
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

    ; Change a valute of a node while
    ; adding new nodes along the way.
    (def (expand path val)
      (if (null? path)
          (set! data val)
        (let ([role (car path)]
              [new-child
               ; Cascade the syncing
               (new mole% [synced-i
                           (map (lam (p) (append1 p role))
                                synced)])])
          (set! chilren
            (cons (cons role new-child)
                  children))
          ; Recursively let the child do the work
          (send new-child
                expand (cdr path) val))))

    ; Just update without error-handling and informing
    (define/public (just-update path val)
      (def m-lu
        (delay (assq (car path) data)))

      (if (null? path)
          (set! data val)
        (if (force m-lu)
            (send m-lu
                  just-update (cdr path) val)
          (expand path val))))

    (define/public (update path val)
      (let/ec return
        (def m-lu
          (delay (assq (car path) data)))

        (if (null? path)
            (cond [(eq? data 'UNKNOWN)
                   (set! data val)]
                  [(uneq? data val)
                   (return 'INCONSISTENT)]
                  [else (return 'NOT-CHANGED)])
          (if (force m-lu)
              ; There is a child of that role
              (send m-lu
                    update (cdr path)
                           val
                           fail-con)
            ; There is no child of that role yet
            (expand path val)))
        ; Inform others about the update to do the same,
        ; but without the error-handling and informing.
        (for-each
          (lam (subject)
            (send subject
                  just-update path val))
          (set-subtract synced exclude))))

    (define/public (sync-with m-other)
      (let/ec return
        (when (eq? m-other this)
          (return 'SELF-SYNC))

        (when (set-member? synced m-other)
          (return 'ALREADY-SYNCED))
        ; If not, then the two sync sets are mutually exclusive

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
                   (lam () (return 'INCONSISTENT)))]
          ; We have new intel for `m-other`
          [(eq? m-other UNKNOWN)
           (send m-other
                 update null
                        data
                        (lam () return 'INCONSISTENT))])

        ; Then we move on to syncing children
      (set-add! synced m-other)))))
