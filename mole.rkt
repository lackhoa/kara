#lang racket
(require "lang/kara.rkt"
         racket/hash)
(provide (all-defined-out)
         (all-from-out))

; ---------------------------------
; Molecules
; ---------------------------------
; Naming convention:
; * roles (and paths) are lowercase,
; * data are capitalized.

; The failure continuation that you're
; confident not gonna be called.
(def (no-fail)
  (raise "I did not expect this to fail..."))

(def mole%
  (class* object% (writable<%>)
    ; Initializer
    (super-new)

    ; Initialization parameters
    (init [data-i 'UNKNOWN])
    (init [children-i null])
    (init [sync-ls-i null])

    ; Fields
    (def data data-i)
    (def children children-i)
    ; The sync list is synced among its items,
    ; and everything is synced wiht itself.
    (def sync-ls
      (cons this sync-ls-i))

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
      (check-pred pair?
                  value
                  "Sync list must a non-empty")
      (set! sync-ls value))

    (define/public (custom-display port)
      (display (cons data children) port))

    (define/public (custom-write port)
      (write (cons data children) port))

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
              [else  #f])))

    ; Tell those in the sync list to do something.
    ; `task` is a function takes a molecule.
    (def (inform task)
      (check-pred procedure? task)
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

    (define/public (add-child role mole)
      (check-pred symbol? role)
      (check-class mole mole%
        "Invalid child")
      (cons! (cons role mole)
             children))

    ; Returns a mapping of the originals to their copies,
    ; without the sync list.
    ; By 'the originals', I mean every single node in the tree.
    (define/public (clone-map)
      ; `env` holds the result.
      (let ([env null]
            [new-me
             (new mole% [data-i data])])
        ; Add the children
        (for-each
          (lam (pair)
            (let* ([role (car pair)]
                   [child (cdr pair)]
                   [cp-res (send child clone-map)])
              (let ([lu (assq child cp-res)])
                (check-not-false lu
                  "`clone-map` does not include root")
                (send new-me
                  add-child role (cdr lu)))
              ; Since molecules are trees,
              ; there won't be any conflict.
              (set! env (append env cp-res))))
          children)
        ; Don't forget to map itself.
        (cons (cons this new-me)
              env)))

    ; The complete cloning interface (works for the root)
    (define/public (copy)
      (let ([cns (clone-map)])
        (for-each
          (lam (pair)
            (def orig (car pair))
            (def clone (cdr pair))
            (send clone
              ; Translate the entire sync list
              ; to the copied version.
              ; This part won't work if the system isn't closed,
              ; i.e., this molecule is linked to something else.
              set-sync-ls
                (map
                  (lam (m)
                    (let ([lu (assq m cns)])
                      (check-not-false lu
                        "Sync list contains a non-child \
                         (maybe you're copying a non-root?)")
                      (cdr lu)))
                  (send orig get-sync-ls))))
          cns)
        ; This will return the copy.
        (cdr (assq this cns))))

    (define/public (just-expand path)
      (unless (null? path)
        (let* ([role (car path)]
               [lu (assq role children)])
          (check-pred symbol? role)
          (if lu
              (send (cdr lu)
                expand (cdr path))
            (let ([new-child (new mole%)])
              (add-child role new-child)
              (send new-child
                just-expand (cdr path)))))))

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
      (check-pred list? path
        "Invalid path")
      (unless (ref path)
        (expand path))
      (let ([m (ref path)])
        (check-class m mole%
          "Something is wrong with `expand`?")
        (send m
          update val (lam () (fail-con)))))

    ; Short-hand for the short-hand that is update-path.
    (define/public (update-role role val fail-con)
      (update-path (list role)
                   val
                   fail-con))

    ; Flush the sync list down to the new descendants.
    (define/public (cascade)
      (recur
        (lam (role c)
          ; First cascade down the immediate children,
          ; if the child is new (when its sync list only has itself).
          (when (< (length (send c get-sync-ls))
                   (length sync-ls))
            (send c
              set-sync-ls
                (map (lam (sy-i)
                       (let ([corres (send sy-i ref role)])
                         (check-class corres mole%)
                         corres))
                     sync-ls)))
          ; Then let them carry over.
          (send c cascade))))

    ; Sync up two molecules that haven't been synced before
    (define/public (sync m-other fail-con)
      (check-class m-other mole%
        "Expected a molecule")
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
               (update other-data
                       no-fail)]
              ; We have new intel for `m-other`
              [(eq? other-data 'UNKNOWN)
               (send m-other
                 update data
                        no-fail)]
              ; Nope, the data are inconsistent.
              [else (escape 'CONFLICT)])

            ; Add the missing children
            (def our-roles
              (list->seteq (get-roles)))
            (def their-roles
              (list->seteq (send m-other get-roles)))
            (set-for-each
              (set-subtract their-roles
                            our-roles)
              (lam (role)
                (update-role role
                             'UNKNOWN
                             no-fail)))
            ; Same thing
            (set-for-each
              (set-subtract our-roles
                            their-roles)
              (lam (role)
                (send m-other
                  update-role role
                              'UNKNOWN
                              no-fail)))

            ; Establish the connection among the top-level.
            ; Note that we don't cascade here,
            ; to preserve symmetry the recursive calls.
            (let ([merge
                   (append sync-ls
                           (send m-other get-sync-ls))])
              (set-sync-ls merge)
              (inform (lam (m)
                        (send m set-sync-ls merge))))

            ; Our work is over: Recursively let all the children sync.
            (check-eqv? (length (get-roles))
                        (length (send m-other get-roles))
                        "The components are not the same, \
                         something is wrong with the code")

            (recur
              (lam (role c)
                (send c
                  sync (send m-other ref role)
                       (lam () (escape 'CONFLICT))))))))
      (when (eq? result 'CONFLICT)
        (fail-con)))

    ; Sync two descendants of this molecules.
    ; If they don't exist, expand them.
    (define/public (sync-path p1 p2 fail-con)
      (unless (ref p1)
        (expand p1))
      (unless (ref p2)
        (expand p2))

      (send (ref p1)
        sync (ref p2) fail-con))))
