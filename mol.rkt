#lang racket
(require "lang/kara.rkt"
         racket/hash
         racket/struct)
(provide (all-defined-out))

;;; mol% <<decompress--compress>> cmol% --visualize>> vmol%

;;; Molcules
(struct mol% (sync  #|at least one path|#
              data  #|#f | symbol|#
              kids  #|mols|#))

(def (new-root)
  (mol% '([]) #f null))

(def (ref root path)
  (match path
    [(list)            root]
    [(cons next rest)  (let ([kids  (mol%-kids root)])
                         (and (<= next (last-index kids))
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
  ;; !! Can be replaced with 'while', but we must extract the path every time.
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
    #|Can be replaced by let, but even better, compose|#
    (set! root (update root p1))
    (set! root (update root p2)))

  (def max-height
    #|watch the height to detect cycle|#
    (max (height (ref root p1))
         (height (ref root p2))))
  (displayln max-height)

  (let loop ([fp1  p1] [fp2  p2])
    (let/ec escape
      (when (member fp1 (ref-sync root fp2))
        #|Save ourselves some time here|#
        (escape root))

      (match* ((ref-data root fp1)
               (ref-data root fp2))
        [(x x)    (void)]
        [(#f d2)  (set! root  (update root fp1 d2))]
        [(d1 #f)  (set! root  (update root fp2 d1))]
        [(_ _)    (escape #f)])

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

      (let ([combined  (set-union (ref-sync root fp1)
                                  (ref-sync root fp2))])
        #|Establish the connections|#
        (set! root
          (do&inform root
                     fp1
                     (lam (m)
                       (struct-copy mol% m [sync combined]))))
        (set! root
          (do&inform root
                     fp2
                     (lam (m)
                       (struct-copy mol% m [sync combined])))))

      (for ([new-fp1 (kids-paths root fp1)]
            [new-fp2 (kids-paths root fp2)])
        ;; Our job is over, let the kids sync
        (match (loop new-fp1 new-fp2)
          [#f        (escape #f)]
          [new-root  (set! root new-root)]))
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
    (mol% (swap-prefix (ref-sync root p)
                       from
                       to)
          (ref-data root p)
          (for/list ([kid-path  (kids-paths root p)])
            (loop kid-path)))))

(def (attach root branch to)
  (replace (update root to)
           to
           (migrate branch '[] to)))

(def (detach root path)
  ;; Convert root-path into a root
  (migrate root path '[]))

(def (pull host to guest)
  ;; It's like updating combined with synchronizing
  (let* ([unifier (new-root)]
         [unifier (attach unifier host  '[0])]
         [unifier (attach unifier guest '[1])])
    (match (sync unifier
                 (append '[0] to)
                 '[1])
      [#f       #f]
      [unified  (detach unified '[0])])))

(def (compress root)
  (let ([dic null  #|Map of sync list to cmol|#])
    (let loop ([mol  root])
      (match (for/or ([pair  dic])
               (if (member (car (mol%-sync mol))
                           (car pair))
                   pair
                   #f))
        [#f            (let ([res  (cmol% (mol%-data mol)
                                          (map loop (mol%-kids mol)))])
                         (cons! (cons (mol%-sync mol)
                                      res)
                                dic)
                         res)]
        [(cons _ any)  any]))))

(def (decompress cmol)
  (let ([topo  (topology cmol)])
    (let loop ([cm    cmol]
               [path  null])
      (mol%  (for/or ([sync-ls topo])
               (if (member path sync-ls)
                   sync-ls
                   #f))
             (cmol%-data cm)
             (for/list ([kid  (cmol%-kids cm)]
                        [i    (in-naturals)])
               (loop kid (pad path i)))))))

;;; Compressed molecule using pointers
(struct cmol% (data kids)
  #:prefab)

(def (topology cmol)
  #|a partition of paths based on value|#
  (def (cascade-path cmol proc)
    ;; proc: cmol -> path -> ?
    (let loop ([mfocus  cmol]
               [path    null])
      (proc mfocus path)
      (let ([kids  (cmol%-kids mfocus)])
        (for ([i    (in-naturals)]
              [kid  kids])
          (loop kid (pad path i))))))

  (let ([result  (make-hasheq)])
    (cascade-path cmol
                  (lam (cm path)
                    (hash-set! result
                               cm
                               (cons path
                                     (hash-ref! result cm null)))))
    (hash-values result)))

;;; Printing
(def (dm c/mol [port  (current-output-port)])
  ;; Write cmol prettily
  (def (listify cmol)
    (let ([dic  (make-hasheq)])
      (let loop ([cm  cmol])
        (hash-ref! dic
                   cm
                   (thunk (cons (cmol%-data cm)
                                (map loop (cmol%-kids cm))))))))

  (parameterize ([print-graph  #t])
    (pdisplay (listify (match c/mol
                         [(mol% _ _ _)  (compress c/mol)]
                         [(cmol% _ _)   c/mol]  #|Compress is necessary|#))
              35
              port)))

(def (wm cmol? port)
  ;; Write cmol efficiently
  (parameterize ([print-graph  #t])
    (write (match cmol?
             [(mol% _ _ _)  (compress cmol?)]
             [(cmol% _ _)   cmol?]  #|Compress if necessary|#)
           port)))

;;; Assignment (NOT mutation)
(define-syntax-rule (update! iden more ...)
  (set! iden (update iden more ...)))

(define-syntax-rule (sync! iden more ...)
  (set! iden (sync iden more ...)))

(define-syntax-rule (pull! iden more ...)
  (set! iden (pull iden more ...)))
