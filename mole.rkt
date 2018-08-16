#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out)
         (all-from-out))

; ---------------------------------
; Molecules
; ---------------------------------
; Naming convention:
; * roles are lowercase,
; * data are capitalized.

(def mole%
  (class object%
    ; Initializer
    (super-new)

    ; Initialization parameters
    (init [data-i 'UNKNOWN])
    (init [children-i null])
    (init [sync-ls-i null])

    ; Fields
    (def data data-i)
    (def children children-i)
    ; The sync list is synced, too
    ; Everything is also synced to itself
    (def sync-ls (cons this sync-ls-i))

    ; Getters
    (define/public (get-children)
      children)
    (define/public (get-data)
      data)
    (define/public (get-roles)
      (map car children))
    (define/public (get-sync-ls)
      sync-ls)

    ; Setters
    (define/public (set-data val)
      (set! data val))
    (define/public (set-sync-ls value)
       (set! sync-ls value))


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


    (define/public (just-expand path)
      (unless (null? path)
        (let* ([role (car path)]
               [lu (assq role children)])
          (if lu
              (send (cdr lu) expand (cdr path))
            (let ([new-child (new mole%)])
              (set! children (cons (cons role new-child)
                                   children))
              (send new-child just-expand (cdr path)))))))

    (define/public (expand path)
      (just-expand path)
      (inform (lam (m)
                (send m just-expand path)))
      ; The ordering is important here: we must add
      ; all the components before referencing in the sync list.
      (cascade)
      (inform (lam (m)
                (send m cascade))))

    ; Updating and syncing the data.
    ; `fail-con`: the function to in case of inconsistency.
    (define/public (update val fail-con)
      (unless (symbol? val)
        (error "UPDATE" "Invalid value" val))
      (when (neq? data val)
        (if (eq? data 'UNKNOWN)
            (begin
              (set! data val)
              (inform
                (lam (m)
                  (send m set-data val))))
          ; Conflict: available data is not equal.
          (fail-con))))

    ; Just a convenience function
    (define/public (update-path path val fail-con)
      (when (eq? (ref path)
                 'NOT-FOUND)
        (expand path))
      (send (ref path)
        update val (lam () (fail-con))))

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
               (update other-data
                       (lam () (raise "Can't fail 1")))]
              ; We have new intel for `m-other`
              [(eq? other-data 'UNKNOWN)
               (send m-other
                 update data
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
                (update-path (list role)
                             'UNKNOWN
                             (lam () (raise "Can't fail 3")))))
            ; Same thing
            (set-for-each
              (set-subtract our-roles
                            their-roles)
              (lam (role)
                (send m-other
                  update-path (list role)
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
