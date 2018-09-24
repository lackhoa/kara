#lang racket
(require "lang/kara.rkt"
         racket/hash)
(provide (all-defined-out))

;;; Molcules
(struct mol% ([data #:mutable  #|'no-dat | symbol|#]
              [kids #:mutable  #|list[mol | valid path]|#])
  #:prefab)

(def (new-mol)
  (mol% 'no-dat null))

(def (ref mol path)
  (match path
    ['[]             mol]
    [(cons nxt rst)  (let ([kids  (mol%-kids mol)])
                       (match (<= nxt (last-index kids))
                         [#t  (ref (list-ref kids nxt)
                                   rst)]
                         [#f  #f]))]))

(def (ref-data mol path)
  (match (ref mol path)
    [#f             'no-dat]
    [(mol% data _)  data]))

(def (ref-kids mol path)
  (match (ref mol path)
    [#f             null]
    [(mol% _ kids)  kids]))

(def (cascade mol proc)
  (let loop ([focus  mol])
    (proc focus)
    (for ([kid  (mol%-kids focus)])
      (loop kid))))

(def (cascade-path mol proc)
  ;; proc: mol -> path -> ?
  (let loop ([mfocus  mol]
             [path    null])
    (proc mfocus path)
    (let ([kids  (mol%-kids mfocus)])
      (for ([i    (in-naturals)]
            [kid  kids])
        (loop kid (pad path i))))))

(def (update! mol path [val 'no-dat])
  ;; Can be used to update (val != 'no-dat)
  ;; or expand (when val = 'no-dat).
  (def (expand! mol next-id)
    (let ([kids (mol%-kids mol)])
      (set-mol%-kids! mol
                      (let ([fillers (build-list (- next-id
                                                    (last-index kids))
                                                 (lam (x)  (mol% 'no-dat null)))])
                        (append kids fillers)))))

  (let loop ([m mol] [p path])
    (match p
      ['[]             (unless (eq? val 'no-dat)
                         (match (mol%-data m)
                           [(== val)  (void)]
                           ['no-dat  (set-mol%-data! m val)]
                           [_        #f]))]
      [(cons nxt rst)  (match (ref mol (list nxt))
                         [#f   (begin (expand! mol nxt)
                                      (update! (ref mol (list nxt))
                                               rst
                                               val))]
                         [kid  (update! kid rst val)])])))


(def (height mol)
  ;; Useful to have
  (match (mol%-kids mol)
    [(list)  0]
    [kids    (add1 (apply max (map height kids)))]))

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

(def (replace! mol path rmol)
  ;; replace path by rmol
  (let*-values ([(pfocus plast)  (split-at-right path 1)]
                [(mfocus)        (ref mol pfocus)])
    (set-mol%-kids! mfocus
                    (list-set (mol%-kids mfocus)
                              (car plast)
                              rmol))))

(def (descendant? branch root)
  (let ([kids  (mol%-kids root)])
    (or (memq branch kids)
       (exists (lam (kid)  (descendant? branch kid))
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

    (when (or (descendant? mol1 mol2)
              (descendant? mol2 mol1))
      (escape #f)  #|Avoid infinite loop|#)

    (let loop ([m1 mol1] [m2 mol2])
      #|Make both molecules similar|#
      (let ([d1  (mol%-data m1)]
            [d2  (mol%-data m2)])
        (match* (d1 d2)
          [(x x)        (void)]
          [('no-dat _)  (update! m1 '[] d2)]
          [(_ 'no-dat)  (update! m2 '[] d1)]
          [(_ _)        (escape #f)]))

      (let ([i1  (last-index (mol%-kids m1))]
            [i2  (last-index (mol%-kids m2))])
        ;; Add the missing kids
        (cond [(< i1 i2)  (update! m1 `[,i2])]
              [(< i2 i1)  (update! m2 `[,i1])]))

      (for ([kid1 (mol%-kids m1)]
            [kid2 (mol%-kids m2)])
        ;; Our job is over, let the kids sync
        (unless (loop kid1 kid2)
          (escape #f))))

    (let ([translator  (make-hasheq)]
          [topo        (merge-topo (topology mol1)
                                   (topology mol2))])
      (displayln "topology1")
      (displayln (topology mol1))
      (displayln "topology2")
      (displayln (topology mol2))
      (displayln "merged")
      (displayln topo)

      (def (process topo)
        (foldr append null topo))
      (assert (= (length (process (topology mol1)))
                 (length (process (topology mol2))))
              "Oh my god how?" (topology mol1) (topology mol2))

      ;; Tricky part: transform the topology of mol1
      (when (cyclic-topo topo)
        (escape #f))

      (for ([chain  topo])
        (let ([mcentral  (ref mol1 (car chain))])
          (for ([p-replaced  (cdr chain)])
            (hash-set! translator
                       (ref mol1 p-replaced)
                       mcentral)

            (replace! mol1
                      p-replaced
                      mcentral))))

      (cascade-path mol2
                    (lam (mol path)
                      (hash-ref! translator
                                 mol
                                 (ref mol1 path)))
                    #|Translate mol2's pointers to mol1's|#)

      (cascade root
               (lam (mol)
                 (unless (eq? mol mol1)
                   (set-mol%-kids! mol
                                   (for/list ([kid  (mol%-kids mol)])
                                     (hash-ref translator kid kid))))
                 #|Erase mol2 entirely, with mol1's extra pointers|#)))))

;;; Functional stuff
(def (copy mol [path null])
  ;; Utilize the print graph ability
  ;; Can be used to detach
  (let-values ([(in out) (make-pipe)])
    (parameterize ([print-graph #t])
      (write (ref mol path) out))
    (read in)))

(def (update mol path [val 'no-dat])
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
(def (pull root branch path)
  (let ([unifier  (mol% 'no-dat
                        `(,(copy root) ,(copy branch)))])
    (match (sync! unifier
                  `[0 ,@path]
                  '[1])
      [#f  #f]
      [_   (copy unifier '[0])])))

(define-syntax-rule (pull! root branch path)
  (set! root (pull root branch path)))

;;; Printing
(def (dm mol [port  (current-output-port)])
  ;; Make it pretty
  (parameterize ([print-graph  #t])
    (pdisplay mol 35 port)))

(def (wm mol port)
  ;; Make it efficient
  (parameterize ([print-graph  #t])
    (write mol port)))
