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
    (init [sync-ls-i null])

    (def data data-i)
    (def children children-i)
    ; The sync list is synced, too
    ; Everything is also synced to itself
    (def sync-ls (cons this sync-ls-i))

    (define/public (get-children)
      children)
    (define/public (get-data)
      data)
    (define/public (get-roles)
      (map car children))
    (define/public (get-sync-ls)
      sync-ls)

    (super-new)

    ; Reference a child.
    (define/public (ref role)
      (let ([mlu (assq role data)])
        (if mlu mlu 'NOT-FOUND)))

    ; Update a value to a child that we don't have yet.
    ; What about the sync list?
    (def (expand path val)
      (if (null? path)
          (set! data val)
        (let* ([role (car path)]
               [new-child (new mole%)])
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
    ; New molecules created are synced.
    (define/public (update path val fail-con)
      (def result
        (let/ec escape
          (if (null? path)
              (when (neq? data val)
                (if (eq? data 'UNKNOWN)
                    (begin (set! data val)
                           (inform-update null val))
                  (lam () (escape 'CONFLICT))))
            (let* ([m-lu (ref (car path))]
                   [not-found? (eq? m-lu 'NOT-FOUND)])
              (if not-found?
                  ; add the child, then tell others
                  (begin
                    (expand path val)
                    (inform-update path val)
                    ; Cascade the sync list AFTER
                    ; the nodes were added
                    (send (ref (car path))
                      cascade sync-ls
                              (car path)))
                ; Delegate task to child
                (send m-lu
                  update (cdr path)
                         val
                         (lam () (escape 'CONFLICT))))))))
      (when (eq? result 'CONFLICT)
        (fail-con)))

    ; Only used in `cascade`
    (define/public (just-cascade sync-list role)
      (set! sync-ls
        (map (lam (mole) (send mole ref role))
             sync-list)))

    (define/public (cascade sync-list role)
      (just-cascade sync-list role)
      (for-each (lam (new-role)
                  (send (ref new-role)
                    cascade sync-ls new-role))
                (get-roles))
      (inform-cascade))

    (define/public (inform-cascade sync-list role)
      (for-each
        (lam (subject)
          (send subject
            just-cascade sync-list role))
        (remq this sync-ls)))

    ; Only used in `update`
    (define/public (inform-update path val)
      (for-each
        (lam (subject)
          (send subject
            just-update path val))
        (remq this sync-ls)))

    (define/public (update-sync-ls sync-list)
      (set! sync-ls sync-list)
      (inform-sync-ls)
      (cascade))

    ; Only used in udpate-sync-ls
    (define/public (inform-sync-ls)
      (for-each
        (lam (subject)
          (send subject update-sync-ls sync-ls))
        (remq this sync-ls)))

    ; Sync up two molecules that haven't been synced before
    (define/public (sync m-other fail-con)
      (def result
        (let/ec escape
          (when (memq m-other sync-ls)
            (escape 'ALREADY-SYNCED))
          ; Fact: The two sync sets are mutually exclusive
          ; Start out with syncing data
          (begin
            (def other-data
              (send m-other get-data))
            (cond
              ; No on the top level
              [(eq? data other-data)
               (void)]
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
            (let ([merge-sync-ls
                   (append sync-ls
                           (send m-other get-sync-ls))])
              (update-sync-ls merge-sync-ls))

            ; Recursively let all the children sync
            (for-each
              (lam (role)
                (send (ref role)
                  sync (send m-other ref role)
                            (lam () (escape 'CONFLICT))))
              (set-union (get-roles)
                         (send m-other get-roles))))))
      (when (eq? result 'CONFLICT)
        (fail-con)))))
