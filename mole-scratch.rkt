(def (mol-repr root [path null])
  (def (blank? mol)
    (match mol
      [(mol% _ 'no-dat (list))  #t]
      [_                        #f]))

  (def (make-repr-env mol
                      in-env
                      get-next!)
    ;; Sort out which variables are
    ;; represented by which symbol.
    (match (blank? mol)
      [#t  (match (hash-ref in-env mol 'unbound)
             ['unbound  (hash-set-many in-env
                                       (map (lam (p)  (ref root p))
                                            (mol%-sync mol))
                                       (get-next!))]
             [_         in-env])]
      [#f  (let ([kids  (mol%-kids mol)]
                 [env   in-env])
             (for ([kid  kids])
               (set! env (make-repr-env kid
                                        env
                                        get-next!)))
             env)]))

  (def (mol-repr-core mol env)
    (match (blank? mol)
      [#t  (hash-ref env mol)]
      [#f  (cons (match (mol%-data mol)
                   ['no-dat  '?]
                   [any      any])
                 (map (lam (kid)  (mol-repr-core kid env))
                      (mol%-kids mol)))]))

  (mol-repr-core (ref root path)
                 (make-repr-env (ref root path)
                                (hash)
                                (let ([count 64])
                                  ;; The counting closure
                                  (thunk (set! count (add1 count))
                                         (integer->char count))))))

(def (update root
             path
             [val  'no-dat])
  ;; Used to update or expand
  ;; if val is 'no-dat, do not overwrite existing data
  (let loop ([rel  #|path so far|# null]
             [path #|path left|#   path])
    (match path
      [(list)  (match* ((ref-data root rel) val)
                 [(any any)    root]
                 [('no-dat _)  (do&inform root
                                          rel
                                          (lam (m)  (struct-copy mol% m [data val])))]
                 [(_ 'no-dat)  root]
                 [(_ _)        'conflict])]

      [(cons next-id rest-path)
       (let ([kids  (ref-kids root rel)])
         (when (< (last-index kids)
                  next-id)
           (set! root
             (do&inform
              root
              rel
              (lam (m)
                (struct-copy mol% m
                             [kids (let* ([missing-indices  (range (add1 (last-index kids))
                                                                   (add1 next-id))]
                                          [fillers  (for/list ([i missing-indices])
                                                      (make-mol (for/list ([p (mol%-sync m)])
                                                                  (pad p i))
                                                                #|empty mole with inherited sync list|#))])
                                     (append kids fillers))]))))))
       (loop (pad rel next-id)
             rest-path)])))

(def (replace mol path new)
  ;; Crucial auxiliary function
  (let loop ([path  path]
             [m     mol])
    (match path
      [(list)               new]
      [(cons next-id rest)
       (struct-copy mol% m
                    [kids  (list-set (mol%-kids m)
                                     next-id
                                     (loop rest (list-ref (mol%-kids m)
                                                          next-id)))])])))

(def (do&inform root path proc)
  ;; returns the root with `proc` done to `ref path` and its associates.
  ;; proc: mol -> mol | 'conflict
  (let/ec escape
    (for ([p  (ref-sync root path)])
      (match (proc (ref root p))
        ['conflict  (escape 'conflict)]
        [new-mol    (set! root (replace root
                                        p
                                        new-mol))]))
    root))
