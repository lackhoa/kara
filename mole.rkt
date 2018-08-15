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
    ; The sync list is synced, too
    ; Everything is also synced to itself
    (def synced synced-i)

    (define/public (get-data)
      data)
    (define/public (get-children)
      children)
    (define/public (get-roles)
      (map car children))
    (define/public (get-synced)
      synced)

    (super-new)

    (define/public (ref path)
      (let ([m-lu
             (delay (assq (car path)
                          data))])
        (cond [(null? path) data]
              [(force m-lu)
               (send m-lu ref (cdr path))]
              [else 'NOT-FOUND])))

    ; Update a value to a child that we don't have yet.
    (def (expand path val)
      (if (null? path)
          (set! data val)
        (let* ([role (car path)]
               [pad-synced
                (map (lam (p) (append1 p role))
                     synced)]
               [new-child
                ; Cascade the syncing
                (new mole% [synced-i pad-synced])])
          (set! children
            (cons (cons role new-child)
                  children))
          ; Recursively let the child do the work
          (send new-child
            expand (cdr path) val))))

    ; Update without error-handling and informing
    (define/public (just-update path val)
      (if (null? path)
          (set! data val)
        (expand path val)))

    ; `fail-con`: the function to in case of inconsistency
    (define/public (update path val fail-con)
      (def result
        (let/ec escape
          (if (null? path)
              (when (uneq? data val)
                (if (eq? data 'UNKNOWN)
                    (begin (set! data val)
                           (inform-update null val))
                  (lam () (escape 'CONFLICT))))
            (let ([m-lu (assq (car path) data)])
              (if (m-lu)
                  ; Delegate task to child
                  (send m-lu
                    update (cdr path)
                           val
                           (lam () (escape 'CONFLICT)))
                ; We add the child, and tell others
                (begin (expand path val)
                       (inform-update path val)))))))
      (when (eq? result 'CONFLICT)
        (fail-con)))

    ; Only used in `update` for syncing
    (define/public (inform-update path val)
      (for-each
        (lam (subject)
          (send subject
            just-update path val))
        (remq this synced)))

    ; Only used in `sync`
    (define/public (inform-sync sync-list)
      (for-each
        (lam (subject)
          (send subject
            just-sync sync-list))
        (remq this synced)))

    (define/public (just-sync sync-list)
      (set! synced sync-list))

    (define/public (sync-with m-other fail-con)
      (def result
        (let/ec escape
          (when (memq m-other synced)
            (escape 'ALREADY-SYNCED))
          ; Fact: The two sync sets are mutually exclusive
          ; Start out with syncing data
          (begin
            (def other-data
               (send m-other get-data))
            (cond
              ; No conflict
              [(eq? data other-data)
               (escape 'NOT-CHANGED)]
              ; `m-other` has new intel for us
              [(eq? data 'UNKNOWN)
               (update null
                       other-data
                       (lam () (escape 'CONFLICT)))]
              ; We have new intel for `m-other`
              [(eq? m-other 'UNKNOWN)
               (send m-other
                 update null
                        data
                        (lam () (escape 'CONFLICT)))])

            ; Add the missing children
            (for-each
              (lam (p)
                (update p
                        (send m-other ref p)
                        (escape 'CONFLICT)))
              (set-subtract (send m-other get-roles)
                            (get-roles)))
            ; Same thing
            (for-each
              (lam (p)
                (send m-other
                  update p
                         (ref p)
                         (escape 'CONFLICT)))
              (set-subtract (get-roles)
                            (send m-other get-roles)))

            ; Establish the connection among the top-level.
            (let ([merge-synced
                   (append synced
                           (send m-other get-synced))])
              (just-sync merge-synced)
              ; Since the sync list has been updated, the
              ; message will go to the other side, too
              (inform-sync merge-synced))

            ; Recursively let all the children sync
            (for-each
              (lam (child)
                (send child
                  sync-with (send m-other ref child)
                            (lam () (escape 'CONFLICT))))
              (set-union (get-roles)
                         (send m-other get-roles))))))
      (when (eq? result 'CONFLICT)
        (fail-con)))))
