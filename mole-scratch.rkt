;;; Macros
(define-syntax update-macro
  (syntax-rules ()
    [(_ mol)  (void)]

    [(_ mol (path ctor) rest ...)
     (begin
       (send mol update-path
         'path ctor)
       (update-macro mol rest ...))]))

(define-syntax sync-macro
  (syntax-rules ()
    [(_m )  (void)]
    [(_ m (path paths ...) rest ...)
     (begin (sync-paths m 'path 'paths ...)
            (sync-macro m rest ...))]))



(def (complexity mol)
  (add1 (sum-list (map complexity
                       (send mol get-kids)))))

(def (ref mol path)
  ;; Reference a descendant.
  (if (null? path)
      mol
      (match (send mol refr (car path))
        ['NOT-FOUND 'NOT-FOUND]
        [kid        (ref kid (cdr path))])))

(def (set-type-path mol path val)
  (send mol expand path no-fail)
  (send (ref mol path) set-type val))

(def (ref-data mol path)
  (match (ref mol path)
    ['NOT-FOUND '?DATA]
    [kid        (send kid get-data)]))

(def (ref-type mol path)
  (match (ref mol path)
    ['NOT-FOUND '?TYPE]
    [kid        (send kid get-type)]))

(def (update-path mol path val
                  [fail-con no-fail])
  (match (let/ec escape
           (match (ref mol path)
             ['NOT-FOUND (send mol expand
                           path
                           (thunk (escape 'FAIL)))
                         (send (ref mol path) update
                           val no-fail)]
             [kid        (send kid update
                           val
                           (thunk (escape 'FAIL)))]))
    ['FAIL  (fail-con)]
    [_      (void)]))

(def (expand-and-get-paths mol paths)
  (for      ([p paths]) (send mol expand p))
  (for/list ([p paths]) (ref mol p)))

(def (sync-paths mol
                 paths
                 [fail-con no-fail])
  (check-false (null? paths)
               "sync-paths must be given than one paths")

  (match (let/ec escape
           (let* ([mols    (expand-and-get-paths mol paths)]
                  [master   (first mols)]
                  [servants (list-tail mols 1)])
             (for ([m servants])
               (send m sync
                 master
                 (thunk (escape 'FAIL))))))
    ['FAIL  (fail-con)]
    [_      (void)]))

(def (preserve mol paths)
  (for ([m  (expand-and-get-paths mol paths)])
    (send m mark-no-touch)))

(def (distinguish mol paths)
  ;; Do not let these molcules sync with each other.
  ;; note: overwrites existing no-sync policies
  ;; note: error checking is left to the user
  (let ([mols (expand-and-get-paths mol paths)])
    (for ([m mols])
      (send m set-no-sync
        (remq m mols)))))

(def (mark-no-touch-paths mol paths)
  (for ([m (expand-and-get-paths mol paths)])
    (send m mark-no-touch)))
