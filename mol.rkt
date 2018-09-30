#lang racket
(require "lang/kara.rkt"
         racket/hash
         racket/struct)
(provide (all-defined-out))

;;; Molcules
;; data    : #f  | symbol
;; kids    : 'no-kid  | mols
;; sync-ls : at least one path
(struct mol% (sync data kids) #:prefab)

(def (new-root)
  (mol% '([]) #f null))

(def (repr root [path null])
  (def (blank? mol)
    (match mol
      [(mol% _ #f (list))  #t]
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

  (def (repr-core mol env)
    (match (blank? mol)
      [#t  (hash-ref env mol)]
      [#f  (cons (match (mol%-data mol)
                   [#f  '?]
                   [any      any])
                 (map (lam (kid)  (repr-core kid env))
                      (mol%-kids mol)))]))

  (repr-core (ref root path)
             (make-repr-env (ref root path)
                            (hash)
                            (let ([count 64])
                              ;; The counting closure
                              (thunk (set! count (add1 count))
                                     (integer->char count))))))

(def (ref root path)
  (match path
    [(list)            root]
    [(cons next rest)  (let ([kids  (mol%-kids root)])
                         (andb (<= next (last-index kids))
                               (ref (list-ref kids next)
                                    rest)))]))

(def (ref-data root path)
  (match (ref root path)
    [#f               #f]
    [(mol% _ dat _)  dat]))

(def (ref-kids root path)
  (match (ref root path)
    [#f                null]
    [(mol% _ _ kids)  kids]))

(def (ref-sync root path)
  (match (ref root path)
    [#f                 (list path)]
    [(mol% sync _ _ )  sync]))

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
  ;; proc: mol -> mol | #f
  (let/ec escape
    (for ([p  (ref-sync root path)])
      (match (proc (ref root p))
        [#f       (escape #f)]
        [new-mol  (set! root (replace root
                                      p
                                      new-mol))]))
    root))

(def (update root
             path
             [val  #f])
  ;; Used to update or expand
  ;; if val is #f, do not overwrite existing data
  (let loop ([rel  #|path so far|# null]
             [path #|path left|#   path])
    (match path
      [(list)  (match* ((ref-data root rel) val)
                 [(x x)   root]
                 [(#f _)  (do&inform root
                                     rel
                                     (lam (m)  (struct-copy mol% m [data val])))]
                 [(_ #f)  root]
                 [(_ _)   #f])]

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
                                                      (mol% (for/list ([p (mol%-sync m)])
                                                              (pad p i))
                                                            #f
                                                            null
                                                            #|empty mole with inherited sync list|#))])
                                     (append kids fillers))]))))))
       (loop (pad rel next-id)
             rest-path)])))

(def (kids-paths root path)
  ;; Very helpful utility.
  (let ([kids-indices  (range (length (ref-kids root path)))])
    (for/list ([i  kids-indices])
      (pad path i))))

(def (height mol)
  ;; Used for synchronization
  (match (mol%-kids mol)
    [(list)  0]
    [kids    (add1 (apply max (map height kids)))]))

(def (sync root p1 p2)
  ;; Establish a new synchronization, expand if needed.
  (begin
    #|Make sure the paths exist|#
    (set! root (update root p1))
    (set! root (update root p2)))

  (def max-height
    #|watch the height to detect cycle|#
    (max (height (ref root p1))
         (height (ref root p2))))

  (let loop ([fp1  p1] [fp2  p2])
    (let/ec escape
      (when (member fp1 (ref-sync root fp2))
        #|Save ourselves some time here|#
        (escape root))

      (match* ((ref-data root fp1)
               (ref-data root fp2))
        [(x x)                (void)]
        [(#f other-dat)  (set! root  (update root fp1 other-dat))]
        [(dat #f)        (set! root  (update root fp2 dat))]
        [(_ _)                (escape #f)])

      (let ([i1  (last-index (ref-kids root fp1))]
            [i2  (last-index (ref-kids root fp2))])
        ;; Add the missing kids
        (cond [(< i1 i2)
               (set! root (update root
                                  (pad fp1 i2)))]

              [(> i1 i2)
               (set! root (update root
                                  (pad fp2 i1)))])

        (when (> (max (height (ref root p1))
                      (height (ref root p2)))
                 max-height)
          (escape #f)))

      (let ([combined  (remove-duplicates
                        (append (ref-sync root fp1)
                                (ref-sync root fp2)))])
        #|Establish the connections|#
        (set! root
          (do&inform root fp1 (lam (m)
                                (struct-copy mol% m [sync combined]))))
        (set! root
          (do&inform root fp2 (lam (m)
                                (struct-copy mol% m [sync combined])))))

      (for ([new-fp1 (kids-paths root fp1)]
            [new-fp2 (kids-paths root fp2)])
        ;; Our job is over, let the kids sync
        (match (loop new-fp1 new-fp2)
          [#f  (escape #f)]
          [new-root   (set! root new-root)]))
      root)))

(def (migrate root from to)
  ;; Migrate the molecule from root-`from` to ?-`to`
  ;; Enables `attach` and `detach`
  (def (swap-prefix ls pre new-pre)
    (for/list ([li  (filter (lam (x) (list-prefix? pre x))
                            ls   #|Weed out the non-descendants|#)])
      (append new-pre
              (list-tail li (length pre)))))

  (let loop ([p  from])
    (mol% (swap-prefix (ref-sync root p)  from  to)
          (ref-data root p)
          (for/list ([kid-path  (kids-paths root p)])
            (loop kid-path)))))

(def (attach root branch to)
  (replace (update root to)
           to
           (migrate branch '[] to)))

(def (detach root from)
  ;; Convert root-path into a root
  (migrate root from '[]))

(def (pull host  guest  [to null])
  ;; It's like synchronizing, but with different roots
  (let* ([unifier (new-root)]
         [unifier (attach unifier host  '[0])]
         [unifier (attach unifier guest '[1])])
    (match (sync unifier
                 (append '[0] to)
                 '[1])
      [#f  #f]
      [unified    (detach unified '[0])])))

(def (dm mol [port (current-output-port)])
  (pdisplay (repr mol) 35 port))


(def (mol%->cmol% mol)
  (let ([dic  (make-hasheq)])
    (let loop ([p  '[]])
      (match (findf (curry member p)
                    (hash-keys dic))
        [#f   (let ([res (cmol% (ref-data p)
                                (map loop (ref-kids p)))])
                (hash-set! (ref-sync p)
                           res)
                res)]
        [any  any]))))

(def (cmol%->mol% mol))

;;; Compressed molecule
(struct cmol% ([data  #| #f | symbol |#]
               [kids  #| mols |#])
  #:prefab)

(def (new-mol)
  (cmol% #f null))

(def (ref mol path)
  (match path
    ['[]             mol]
    [(cons nxt rst)  (let ([kids  (cmol%-kids mol)])
                       (and (<= nxt (last-index kids))
                          (ref (list-ref kids nxt) rst)))]))

(def (ref-data mol path)
  (match (ref mol path)
    [#f             #f]
    [(cmol% data _)  data]))

(def (ref-kids mol path)
  (match (ref mol path)
    [#f             null]
    [(cmol% _ kids)  kids]))

(def (cascade mol proc)
  (let loop ([focus  mol])
    (proc focus)
    (for ([kid  (cmol%-kids focus)])
      (loop kid))))

(def (cascade-path mol proc)
  ;; proc: mol -> path -> ?
  (let loop ([mfocus  mol]
             [path    null])
    (proc mfocus path)
    (let ([kids  (cmol%-kids mfocus)])
      (for ([i    (in-naturals)]
            [kid  kids])
        (loop kid (pad path i))))))

(def (update! mol path [val #f])
  ;; Can be used to update (val != #f)
  ;; or expand (when val = #f).
  (def (expand! mol next-id)
    (let ([kids (cmol%-kids mol)])
      (set-cmol%-kids! mol
                      (let ([fillers (build-list (- next-id
                                                    (last-index kids))
                                                 (lam (x)  (cmol% #f null)))])
                        (append kids fillers)))))

  (let loop ([m mol] [p path])
    (match p
      ['[]             (unless (eq? val #f)
                         (match (cmol%-data m)
                           [(== val)  (void)]
                           [#f       (set-cmol%-data! m val)]
                           [_        #f]))]
      [(cons nxt rst)  (match (ref mol `[,nxt])
                         [#f   (begin (expand! mol nxt)
                                      (update! (ref mol `[,nxt])
                                               rst
                                               val))]
                         [kid  (update! kid rst val)])])))

(def (height mol)
  ;; Useful to have
  (match (cmol%-kids mol)
    ['()   0]
    [kids  (add1 (apply max (map height kids)))]))

(def (topology mol)
  #|a partition of paths based on value|#
  (let ([result  (make-hasheq)])
    (cascade-path mol
                  (lam (m path)
                    (hash-set! result
                               m
                               (cons path
                                     (hash-ref! result m null)))))
    (hash-values result)))

(def (get-chain topo path)
  (for/or ([chain  topo])
    (match (member path chain)
      [#f  #f]
      [_   chain])))

(def (synced? topo path1 path2)
  (match (member path2
                 (get-chain topo path1))
    [#f  #f]
    [_   #t]))

(def (mol-paths mol)
  (let ([result  null])
    (cascade-path mol (lam (mol path)
                        (cons! path result)))

    result))

(def (descendant? branch root)
  (let ([kids  (cmol%-kids root)])
    (orb (memq branch kids)
         (findf (lam (kid)  (descendant? branch kid))
                kids))))

(def (cyclic-topo topo)
  (for/or ([chain topo])
    (let* ([sorted  (sort chain
                          #:key length <)])
      (let loop ([sorted sorted])
        #|Looking out for prefix|#
        (match sorted
          ['()             #f]
          [(cons fst rst)  (unless (findf (lam (p)  (list-prefix? fst p))
                                          rst)
                             (loop rst))])))))

(def (merge-topo topo1 topo2)
  (let ([topo    (append topo1 topo2)]
        [result  null])
    (let loop ()
      (unless (null? topo)
        (let ([new-chain  (car topo)])
          (let gather ()
            (match (index-where topo
                                (lam (c)
                                  ((negate set-empty?)
                                   (set-intersect new-chain c))))
              [#f  (void)]
              [i   (set! new-chain (set-union new-chain
                                              (list-ref topo i)))
                   (set! topo (drop-pos topo i))
                   (gather)]))
          (cons! new-chain result))
        (loop)))
    result))

(def (compare m1 m2)
  (match* ((cmol%-data m1)
           (cmol%-data m2))
    [(x x)  (let ([kids1  (cmol%-kids m1)]
                  [kids2  (cmol%-kids m2)])
              (match (length kids1)
                [(== (length kids2))  (for/and ([kid1  kids1]
                                                [kid2  kids2])
                                        (compare kid1 kid2))]
                [_                   #f]))]
    [(a b)  #f]))

(def (exchange! root p pr)
  (let ([m   (ref root p)]
        [mr  (ref root pr)])
    (unless (eq? m mr)
      (let ([translator  (make-hasheq)])
        (cascade-path m
                      (lam (mol path)
                        (assert (ref mr path)
                                "Trying to exchange two paths that are not equal"
                                root p pr)
                        (hash-set! translator
                                   mol
                                   (ref mr path)))
                      #|Building the translator|#)
        (cascade root
                 (lam (mol)
                   (set-cmol%-kids! mol
                                   (for/list ([kid  (cmol%-kids mol)])
                                     (hash-ref translator kid kid)))
                   #|Erase|#))))))

(def (sync! root path1 path2)
  ;; Merge two molecules, if fail, returns #f,
  ;; if successful, literally assign path2 to path1
  (begin
    #|Make sure the paths exist|#
    (update! root path1)
    (update! root path2))

  (let/ec escape
    (def mol1  (ref root path1))
    (def mol2  (ref root path2))

    (when (eq? mol1 mol2)
      (escape 'vacuous)  #|Save some time|#)

    (let* ([max-height  (max (height mol1)
                             (height mol2))]
           [topo1       (topology mol1)]
           [topo2       (topology mol2)]
           [topo        (merge-topo topo1 topo2)])
      (let super-loop ()
        (let loop ([m1 mol1]
                   [m2 mol2])
          #|Make both molecules similar|#
          (let ([d1  (cmol%-data m1)]
                [d2  (cmol%-data m2)])
            (match* (d1 d2)
              [(x x)        (void)]
              [(#f _)  (update! m1 '[] d2)]
              [(_ #f)  (update! m2 '[] d1)]
              [(_ _)        (escape #f)]))

          (let ([i1  (last-index (cmol%-kids m1))]
                [i2  (last-index (cmol%-kids m2))])
            ;; Add the missing kids
            (cond [(< i1 i2)  (update! m1 `[,i2])]
                  [(< i2 i1)  (update! m2 `[,i1])]))

          (when (> (max (height mol1)
                        (height mol2))
                   max-height  #|Infinite cycle|#)
            (escape #f))

          (for ([kid1 (cmol%-kids m1)]
                [kid2 (cmol%-kids m2)])
            ;; Our job is over, let the kids sync
            (unless (eq? kid1 kid2)
              (loop kid1 kid2))))

        (unless (compare mol1 mol2)
          (super-loop)))

      (for ([chain  topo])
        (let ([p-central  (append path1 (car chain))])
          (for ([p-replaced  (map (lam (p)  (append path1 p))
                                  (cdr chain))])
            (exchange! root
                       p-replaced
                       p-central)))
        #|Tricky part: transform mol1's topology|#)

      (exchange! root path2 path1
                 #|Erase path2 from root|#))))

;;; Functional stuff
(def (copy mol [path null])
  ;; Utilize the print graph
  ;; Can be used to detach molecules from the root.
  (let-values ([(in out) (make-pipe)])
    (parameterize ([print-graph #t])
      (write (ref mol path) out))
    (read in)))

(def (update mol path [val #f])
  (let ([clone  (copy mol)])
    (match (update! clone path val)
      [#f  #f]
      [_   clone])))

(def (sync root path1 path2)
  (let ([clone  (copy root)])
    (match (sync! clone  path1  path2)
      [#f  #f]
      [_   clone])))

;;; Others
(def (pull root path branch)
  (let ([unifier  (cmol% #f
                         `(,(copy root) ,(copy branch)))])
    (match (sync! unifier
                  `[0 ,@path]
                  '[1])
      [#f  #f]
      [_   (ref unifier '[0])])))

(define-syntax-rule (pull! root path branch)
  (set! root (pull root path branch)))

;;; Printing
(def (dm mol [port  (current-output-port)])
  ;; Make it pretty
  (parameterize ([print-graph  #t])
    (pprint (cmol%->vcmol% mol) 35 port)))

(def (wm mol port)
  ;; Make it efficient
  (parameterize ([print-graph  #t])
    (write mol port)))

(def (cmol%->vcmol% mol)
  (let ([dic  (make-hasheq)])
    (let loop ([m  mol])
      (hash-ref! dic
                 m
                 (thunk (vcmol% (cmol%-data m)
                                (map loop (cmol%-kids m))))))))

(struct vcmol% (data kids)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer (lam (obj) (match (vcmol%-data obj)
                                                [#f   '?]
                                                [any  any]))
                                     (lam (obj) (vcmol%-kids obj))))])
