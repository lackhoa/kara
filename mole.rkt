#lang racket
(require "lang/kara.rkt"
         "types.rkt")
(provide (all-defined-out))

; ---------------------------------
; Molecules
; ---------------------------------

; The failure continuation that you're
; confident not gonna be called.
(def (no-fail)
  (error "I did not expect this to fail!"))

(def mole%
  (class* object%
          (writable<%>)
    ; Initializer
    (super-new)

    ; Initialization parameters
    (init [data-i    '?DATA])
    (init [kids-i    null])
    (init [sync-ls-i null])

    ; Fields
    (def data data-i)

    (def kids kids-i)

    ; The sync list is synced among its items,
    ; and everything is synced with itself.
    (def sync-ls
      (cons this sync-ls-i))

    ; Getters
    (define/public (get-kids)
      kids)
    (define/public (get-data)
      data)
    (define/public (get-roles)
      (map car kids))

    (define/public (get-sync-ls)
      sync-ls)

    (define/public (get-ctor)
      (refr-data 'ctor))

    (define/public (get-type)
      (refr-data 'type))

    (define/public (ref-ctor path)
      (ref-data (append1 path 'ctor)))

    (define/public (ref-type path)
      (ref-data (append1 path 'type)))

    ; Setters
    (define/public (set-data val)
      (match* (data val kids)
        ; No new information
        [(_ '?DATA _)
         (void)]

        ; Can update
        [(_ _ '())
         (set! data val)]

        ; Illegal state
        [(_ _ _)
         (error "A composite cannot have data")]))

    (define/public (set-sync-ls value)
      (check-pred pair? value
        "Sync list must be non-empty")
      (set! sync-ls value))

    (define/public (add-kid role mole)
      (check-pred symbol? role
        "Role is a symbol")
      (check-class mole mole%
        "Kid is a molecule")
      (check-eq? data '?DATA
        "This is not an atom")
      (cons! (cons role mole)
             kids))

    (define/public (repr)
      (match* (data kids)
        ; Totally unsure whether this is a structure or an atom.
        [('?DATA '()) '?]

        ; This is an atom
        [(_ '()) data]

        ; This is a molecule.
        [('?DATA _)
         (match* ((get-ctor)
                  (get-type))
          ; Unknown constructor, unknown type
          [('?DATA
            '?DATA)
           (cons '? kids)]

          ; Unknown constructor, known type
          [('?DATA
            (Union ctors))
           (cons (stream->list ctors)
                 (filter-not
                   (lam (kid)
                     (case (car kid)
                       [(type ctor) #t]
                       [else        #f]))
                   kids))]

          ; Known constructor
          [((Ctor repr _ _ _)
            _)
           (match repr
             [(Repr leader roles)
              (if (null? roles)
                  leader
                (cons leader
                      (map (lam (role)
                             (match (refr role)
                               ['NOT-FOUND '?]
                               [kid (send kid repr)]))
                           roles)))])])]

        ; Non-trivial data and kids.
        [(_ _)
         (error "Branches cannot have data." data)]))

    (define/public (custom-display port)
      (display (repr) port))

    (define/public (custom-write port)
      (write (repr) port))

    ; Like `ref`, but reference a direct kid
    (define/public (refr role)
      (let ([lu (assq role kids)])
        (cond [lu (cdr lu)]
              [else 'NOT-FOUND])))

    ; Reference a descendant.
    (define/public (ref path)
      (if (null? path)
          this
        (match (refr (car path))
          ['NOT-FOUND 'NOT-FOUND]

          [kid (send kid
                   ref (cdr path))])))

    ; Reference the data of a kid.
    (define/public (ref-data path)
      (match (ref path)
        ['NOT-FOUND '?DATA]

        [kid (send kid get-data)]))

    (define/public (refr-data path)
      (match (refr path)
        ['NOT-FOUND '?DATA]

        [kid (send kid get-data)]))

    ; Tell those in the sync list to do something.
    ; `task` is a function takes a molecule.
    (def (inform task)
      (check-pred procedure? task)
      (for-each
        (lam (subject)
          (task subject))
        (remq this sync-ls)))

    ; Order the kids to do something.
    ; `task` takes a role and a molecule
    (define-syntax-rule (recur task)
      (for-each
        (lam (role)
          (task role (refr role)))
        (get-roles)))

    ; Returns a mapping of the originals to their copies,
    ; without the sync list.
    ; By 'the originals', I mean every single node in the tree.
    (define/public (clone-map)
      ; `env` holds the result.
      (let ([env null]
            [new-me
             (new mole% [data-i data])])
        ; Add the kids
        (for-each
          (lam (pair)
            (let* ([role (car pair)]
                   [kid (cdr pair)]
                   [cp-res (send kid clone-map)])
              (let ([lu (assq kid cp-res)])
                (check-not-false lu
                  "`clone-map` does include the root.")
                (send new-me
                  add-kid role (cdr lu)))
              ; Since molecules are trees,
              ; there won't be any conflict.
              (set! env (append env cp-res))))
          kids)
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
        ; This will return the root's copy.
        (cdr (assq this cns))))

    ; Keep adding new kids
    ; until the path is exhausted.
    (define/public (just-expand path)
      (unless (null? path)
        (let ([new-kid (new mole%)])
          (add-kid (car path) new-kid)
          (send new-kid
            just-expand (cdr path)))))

    (define/public (expand path)
      (let ([lu (refr (car path))])
        (match lu
          ['NOT-FOUND
           (just-expand path)
           (inform (lam (m) (send m just-expand path)))
           ; The order is important: we must add all
           ; components to refer to them in the sync list.
           (cascade path)
           (inform (lam (m) (send m cascade path)))]

          [kid
           (send kid expand (cdr path))])))

    ; Updating and syncing the data.
    ; `fail-con`: the function to in case of inconsistency.
    (define/public (update val
                           [fail-con no-fail])
      (when (neq? data val)
        (match data
          ['?DATA
           (set-data val)
           (inform
             (lam (m)
               (send m set-data val)))]

          ; Conflict: available data is not equal.
          [_ (fail-con)])))

    ; Just a convenience function
    (define/public (update-path path
                                val
                                [fail-con no-fail])
      (match (ref path)
        ['NOT-FOUND
         (expand path)
         (send (ref path)
           update val (lam () (fail-con)))]

        [kid
         (send kid
           update val (lam () (fail-con)))]))

    ; Short-hand for the short-hand that is update-path.
    (define/public (update-role role
                                val
                                [fail-con no-fail])
      (update-path (list role)
                   val
                   fail-con))

    ; Flush the sync list down to the
    ; new descendants on a path.
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
                      "This kid is new")
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
          (match* (data other-data)
            ; No conflict on the top level
            [(x x) (void)]

            ; `m-other` has new intel for us
            [('?DATA _)
             (update other-data)]

            ; We have new intel for `m-other`
            [(_ '?DATA)
             (send m-other update data)]

            ; Nope, the data are inconsistent.
            [(_ _) (escape 'CONFLICT)])

            ; Add the missing kids
            (let ([our-roles
                   (list->seteq (get-roles))]
                  [their-roles
                   (list->seteq (send m-other get-roles))])
              ; Add the missing kids for us
              (set-for-each
                (set-subtract their-roles our-roles)
                (lam (role)
                  (update-role role '?DATA)))

              ; Same thing for the other guy
              (set-for-each
                (set-subtract our-roles their-roles)
                (lam (role)
                  (send m-other
                    update-role role '?DATA))))


            ; Establish the connection among the top-level.
            ; Note that we don't cascade here,
            ; to preserve symmetry the recursive calls.
            (let ([merge
                   (append sync-ls
                           (send m-other get-sync-ls))])
              (set-sync-ls merge)
              (inform
                (lam (m)
                  (send m set-sync-ls merge))))

            ; Our work is over: Recursively let all the kids sync.
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
      (when (eq? 'NOT-FOUND (ref p1))
        (expand p1))
      (when (eq? 'NOT-FOUND (ref p2))
        (expand p2))

      (send (ref p1)
        sync (ref p2) fail-con))))


; Update the constructor of multiple paths of a molecule.
(define-syntax update-ctors
  (syntax-rules ()
    [(_ mole) (void)]

    [(_ mole (path cval) rest ...)
     (begin (send mole
              update-path (append1 (path-proc 'path)
                                   'ctor)
                          cval)
            (update-ctors mole rest ...))]))
