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

    (define/public (repr)
      (cons data (map (lam (c)
                        (cons (car c)
                              (send (cdr c) repr)))
                      children)))

    ; Reference a child.
    (define/public (ref role/path)
      (let* ([path
              (if (list? role/path)
                  role/path
                (list role/path))]
             [lu
              (delay (assq (car path) children))])
        (cond [(null? path)  this]
              [(force lu)
               ; `(cdr (force lu))` is a child
               (send (cdr (force lu))
                 ref (cdr path))]
              [else  'NOT-FOUND])))

    ; Tell those in the sync list to do something.
    ; `task` takes a molecule.
    ; `task` takes a molecule.
    (def (inform task)
      (for-each
        (lam (subject)
          (task subject))
        (remq this sync-ls)))

    ; Order the children to do something.
    ; `task` takes a role and a molecule
    (define-syntax-rule (recur task)
      (for-each
        (lam (role)
          (task role (ref role)))
        (get-roles)))

    ; Update without error-handling and informing
    (define/public (just-update path val)
      (if (null? path)
          (set! data val)
        (let* ([role (car path)]
               [new-child (new mole%)])
          (set! children
            (cons (cons role new-child)
                  children))
          ; Recursively let the child do the work
          (send new-child
            just-update (cdr path) val))))

    ; Update the data at `path`.
    ; `fail-con`: the function to in case of inconsistency
    ; New molecules created are synced.
    (define/public (update path val fail-con)
      (def result
        (let/ec escape
          (unless (symbol? val)
            (error "UPDATE" "Invalid value" val))
          (if (null? path)
              (when (neq? data val)
                (if (eq? data 'UNKNOWN)
                    (begin
                      (set! data val)
                      (inform
                        (lam (m)
                          (send m just-update null val))))
                  ; Conflict: available data is not equal.
                  (escape 'CONFLICT)))
            (let* ([m-lu (ref (car path))]
                   [not-found? (eq? m-lu 'NOT-FOUND)])
              (if not-found?
                  ; add the child, then tell others
                  (begin
                    (just-update path val)
                    (inform (lam (m)
                              (send m just-update
                                path val)))
                    ; Cascade the sync list AFTER
                    ; the nodes were added, otherwise
                    ; no references can be made.
                    (cascade)
                    (inform (lam (m)
                              (send m cascade))))
                ; Not our problem: Delegate task to child
                (send m-lu
                  update (cdr path)
                         val
                         (lam () (escape 'CONFLICT))))))))
      (when (eq? result 'CONFLICT)
        (fail-con)))

    ; Flush the sync list down to the descendants
    (define/public (cascade)
      (recur
        (lam (role c)
          ; First cascade down the immediate children.
          (send c
            set-sync-ls (map (lam (sy-i)
                               (send sy-i ref role))
                              sync-ls))
          ; Then let them carry over.
          (send c cascade))))

    (define/public (set-sync-ls value)
       (set! sync-ls value))

    ; Sync up two molecules that haven't been synced before
    (define/public (sync m-other fail-con)
      (def result
        (let/ec escape
          (unless (is-a? m-other mole%)
            (error "SYNC" "Expected a molecule" m-other))

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
                       (lam () (raise "Can't fail 1")))]
              ; We have new intel for `m-other`
              [(eq? other-data 'UNKNOWN)
               (send m-other
                 update null
                        data
                        (lam () (raise "Can't fail 2")))]
              ; Nope, the data are inconsistent.
              [else (escape 'CONFLICT)])

            ; Add the missing children
            (def our-roles   (list->seteq (get-roles)))
            (def their-roles (list->seteq (send m-other get-roles)))
            (set-for-each
              (set-subtract their-roles
                            our-roles)
              (lam (role)
                (update (list role)
                        'UNKNOWN
                        (lam () (raise "Can't fail 3")))))
            ; Same thing
            (set-for-each
              (set-subtract our-roles
                            their-roles)
              (lam (role)
                (send m-other
                  update (list role)
                         'UNKNOWN
                         (lam () (raise "Can't fail 4")))))

            ; Establish the connection among the top-level.
            (let ([merge
                   (append sync-ls
                           (send m-other get-sync-ls))])
              (set-sync-ls merge)
              (inform (lam (m)
                        (send m set-sync-ls merge))))

            ; Our work is over: Recursively let all the children sync.
            ; Assert that all the components are the same.
            (unless (= (length (get-roles))
                       (length (send m-other get-roles)))
              (raise "Something is wrong"))

            (recur
              (lam (role c)
                (send c
                  sync (send m-other ref role)
                       (lam () (escape 'CONFLICT))))))))
      (when (eq? result 'CONFLICT)
        (fail-con)))))
