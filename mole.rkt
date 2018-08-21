#lang racket
(require "lang/kara.rkt"
         "types.rkt")
(provide (all-defined-out))

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
                  "Sync list must be non-empty")
      (set! sync-ls value))

    (define/public (get-repr)
      (car (tech-get-repr null)))

    ; env: technical parameter to name free variables,
    ; it is a mapping of sync lists to variable names.
    ; Returns: the representation and the new environment.
    (define/public (tech-get-repr env)
      (match data
        ['UNKNOWN
         (cons (cons '? children)
               env)]

        ['ANY
         (check-eq? children null)
         ; Check if this is already in any sync list.
         (match (memf (lam (pair)
                        (assq this (car pair)))
                      env)
           ; No: get a new name and change the environment.
           [#f
            (let* ([next-name
                    (string-append "?"
                      (number->string (length env)))]
                   [new-env
                    (cons (cons sync-ls next-name)
                          env)])

              (cons next-name new-env))]
           ; There is already a name, use it.
           [(list first _) (cons (cdr first)
                                 env)])]

        [(Ctor repr _ _ _)
         (match repr
           [(Repr leader paths)
            (if (null? paths)
                (cons leader env)
              ; new-env keeps the state of the environment.
              ; as we cycle through each components.
              (let ([new-env env])
                (cons (cons leader
                            (map (lam (path)
                                   (match (ref path)
                                     [#f (cons '? new-env)]
                                     [child
                                      (let ([return (send child
                                                      get-repr new-env)])
                                        (set! new-env (cdr return))
                                        (car return))]))
                                 paths))
                      new-env)))])]
        [(Union ctors)
         (cons (cons (force ctors)
                     children)
               env)]

        [other (cons (cons other children)
                     env)]))

    (define/public (custom-display port)
      (display (get-repr) port))

    (define/public (custom-write port)
      (write (get-repr) port))

    ; Reference a descendant. If not found then #f.
    (define/public (ref path)
      (check-pred list? path
        "Path must be a list")

      (if (null? path)
          this
        (match (refr (car path))
          [#f #f]
          ; `(cdr pair`) is a molecule
          [child (send child
                   ref (cdr path))])))

    ; Like `ref`, but reference a direct child
    (define/public (refr role)
      (check-pred symbol? role)

      (let ([lu (assq role children)])
        (cond [lu (cdr lu)]
              [else #f])))

    ; Reference the data of a child.
    (define/public (ref-data path)
      (match (ref path)
        [#f 'UNKNOWN]
        [child (send child get-data)]))

    (define/public (refr-data path)
      (match (refr path)
        [#f 'UNKNOWN]
        [child (send child get-data)]))

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
          (task role (refr role)))
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
                  "`clone-map` does include the root.")
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
                        "Sync list contains only \
                         descendants of this molecule.")
                      (cdr lu)))
                  (send orig get-sync-ls))))
          cns)
        ; This will return the copy.
        (cdr (assq this cns))))

    ; Keep adding new children
    ; until the path is exhausted.
    (define/public (just-expand path)
      (unless (null? path)
        (let ([new-child (new mole%)])
          (add-child (car path) new-child)
          (send new-child
            just-expand (cdr path)))))

    (define/public (expand path)
      (let ([lu (refr (car path))])
        (match lu
          [#f
           (just-expand path)
           (inform (lam (m) (send m just-expand path)))
           ; The order is important: we must add all
           ; components to refer to them in the sync list.
           (cascade path)
           (inform (lam (m) (send m cascade path)))]

          [child
           (send child expand (cdr path))])))

    ; Updating and syncing the data.
    ; `fail-con`: the function to in case of inconsistency.
    (define/public (update val
                           [fail-con no-fail])
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
    (define/public (update-path path
                                val
                                [fail-con no-fail])
      (check-pred list? path
        "Invalid path")
      (unless (ref path)
        (expand path))
      (let ([m (ref path)])
        (check-class m mole%
          "Expand gave us a reference to the path.")
        (send m
          update val (lam () (fail-con)))))

    ; Short-hand for the short-hand that is update-path.
    (define/public (update-role role
                                val
                                [fail-con no-fail])
      (update-path (list role)
                   val
                   fail-con))

    (define/public (mutate val)
      (when (neq? data val)
        (set! data val)
        (inform
          (lam (m)
            (send m set-data val)))))

    (define/public (mutate-path path val)
      (check-pred list? path
        "Invalid path")

      (let ([m (ref path)])
        (check-true (ref path)
          "Mutating an existing path")
        (send m mutate val)))

    (define/public (mutate-role role val)
      (check-pred symbol? role)

      (let ([m (refr role)])
        (check-not-false m
          "Mutating an existing path")
        (send m mutate val)))

    ; Flush the sync list down to the new descendants.
    (define/public (cascade path)
      ; Only work when we have something in the sync list.
      (when (and (not (null? path))
                 (> (length sync-ls) 1))
        (let* ([role (car path)]
               [c    (refr role)])
          (check-class c mole%
                       "We have this path")
          (check-eqv? (length (send c get-sync-ls))
                      1
                      "This child is new")
          (send c
            set-sync-ls
              (map (lam (sy-i) (send sy-i refr role))
                   sync-ls))
          ; Then let them carry over.
          (send c cascade (cdr path)))))

    ; Sync up two molecules that haven't been synced before
    (define/public (sync m-other
                         [fail-con no-fail])
      (check-class m-other mole%
        "Expected a molecule")
      (def result
        (let/ec escape
          (when (memq m-other sync-ls)
            (escape 'ALREADY-SYNCED))

          ; Fact: The two sync sets are mutually exclusive
          ; Start out with syncing data
          (def other-data
            (send m-other get-data))
          (cond
            ; No on the top level
            [(eq? data other-data)
             (void)]
            ; `m-other` has new intel for us
            [(eq? data 'UNKNOWN)
             (update other-data)]
            ; We have new intel for `m-other`
            [(eq? other-data 'UNKNOWN)
             (send m-other update data)]
            ; Nope, the data are inconsistent.
            [else (escape 'CONFLICT)])

            ; Add the missing children
            (def our-roles
              (list->seteq (get-roles)))
            (def their-roles
              (list->seteq (send m-other get-roles)))
            (set-for-each
              (set-subtract their-roles our-roles)
              (lam (role)
                (update-role role 'UNKNOWN)))
            ; Same thing
            (set-for-each
              (set-subtract our-roles their-roles)
              (lam (role)
                (send m-other
                  update-role role 'UNKNOWN)))

            ; Establish the connection among the top-level.
            ; Note that we don't cascade here,
            ; to preserve symmetry the recursive calls.
            (let ([merge
                   (append sync-ls
                           (send m-other get-sync-ls))])
              (set-sync-ls merge)
              (inform (lam (m) (send m set-sync-ls merge))))

            ; Our work is over: Recursively let all the children sync.
            (check-eqv? (length (get-roles))
                        (length (send m-other get-roles))
                        "The structures are synced.")
            (recur
              (lam (role c)
                (send c
                  sync (send m-other refr role)
                       (lam () (escape 'CONFLICT)))))))
      (when (eq? result 'CONFLICT)
        (fail-con)))

    ; Sync two descendants of this molecules.
    ; If they don't exist, expand them.
    (define/public (sync-path p1
                              p2
                              [fail-con no-fail])
      (unless (ref p1)
        (expand p1))
      (unless (ref p2)
        (expand p2))

      (send (ref p1)
        sync (ref p2) fail-con))))


; Update multiple paths of a molecule.
(define-syntax update-macro
  (syntax-rules ()
    [(_ mole) (void)]

    [(_ mole (path ctor) rest ...)
     (begin (send mole
              update-path (path-proc 'path) ctor)
            (update-macro mole rest ...))]))
