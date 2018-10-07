#lang racket
(require "lang/kara.rkt"
         racket/hash
         racket/struct)
(provide (all-defined-out))

;;; mol% <<decompress--compress>> cmol% --visualize>> vmol%

;;; Molcules
(def (mol% sync data kids)
  ;; sync:  at least one path
  ;; data:  #f | symbol
  ;; kids:  mols
  (list sync data kids))

(def mol%-sync first)
(def (mol%-set-sync mol val)
  (list-set mol 0 val))

(def mol%-data second)
(def (mol%-set-data mol val)
  (list-set mol 1 val))

(def mol%-kids third)
(def (mol%-set-kids mol val)
  (list-set mol 2 val))

(def new-root
  '(([])  #f  ()))

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
    [(list _ dat _)   dat]))

(def (ref-kids root path)
  (match (ref root path)
    [#f               null]
    [(list _ _ kids)  kids]))

(def (ref-sync root path)
  (match (ref root path)
    [#f                (list path)]
    [(list sync _ _ )  sync]))

(def (replace mol paths new)
  ;; Crucial auxiliary function
  ;; No path can contain another
  (match paths
    ['([])  new  #|Directed to root|#]
    [_
     (let loop ([paths  paths  #|Will be shortened every cycle|#]
                [m      mol])
       (mol%-set-kids m
                      (for/list ([kid  (mol%-kids m)]
                                 [i    (in-naturals)])
                        (match (map cdr
                                    (filter (lam (p)  (eq? (car p) i))
                                            paths  #|No path can be empty|#))
                          ['()        kid]
                          ['([])      new]
                          [new-paths  (loop new-paths kid)]))))]))

(def (do&inform root path proc)
  ;; returns the root with `proc` done to `ref path` and its associates.
  ;; proc: mol -> mol | #f
  (let ([sl  (ref-sync root path)])
    (match (proc (ref root (car sl)))
      [#f       #f]
      [new-mol  (replace root
                         sl
                         new-mol)])))

(def (update root
             path
             [val  #f])
  ;; Update (val != #f) or Expand (val = #f)
  (def (pad-kids mol kids last-id)
    (mol%-set-kids mol
                   (let* ([missing  (range (add1 (last-index kids))
                                           (add1 last-id))]
                          [fillers  (for/list ([i  missing])
                                      (mol% (for/list ([p  (mol%-sync mol)])
                                              (rcons p i))
                                            #f
                                            null
                                            #|new mole inheriting sync list|#))])
                     (append kids fillers))))

  (let loop ([rel  #|path so far|# null]
             [path #|path left|#   path]
             [root #|root state|#  root])
    (match path
      [(list)  (match* ((ref-data root rel) val)
                 [(x x)   root]
                 [(#f _)  (do&inform root
                                     rel
                                     (lam (m)  (mol%-set-data m val)))]
                 [(_ #f)  root]
                 [(_ _)   #f])]

      [(cons next-id rest-path)
       (let ([kids  (ref-kids root rel)])
         (loop (rcons rel next-id)
               rest-path
               (match (< (last-index kids)
                         next-id  #|Need to expand?|#)
                 [#f  root]
                 [#t  (do&inform root
                                 rel
                                 (lam (m)
                                   (pad-kids m kids next-id)))])))])))

(def (kids-paths root path)
  ;; Very helpful utility.
  (let ([kids-indices  (range (length (ref-kids root path)))])
    (for/list ([i  kids-indices])
      (rcons path i))))

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
        [(x x)    (void)]
        [(#f d2)  (set! root  (update root fp1 d2))]
        [(d1 #f)  (set! root  (update root fp2 d1))]
        [(_ _)    (escape #f)])

      (let ([i1  (last-index (ref-kids root fp1))]
            [i2  (last-index (ref-kids root fp2))])
        ;; Add the missing kids
        (cond [(< i1 i2)
               (set! root (update root
                                  (rcons fp1 i2)))]

              [(> i1 i2)
               (set! root (update root
                                  (rcons fp2 i1)))])

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
                       (mol%-set-sync m combined))))
        (set! root
          (do&inform root
                     fp2
                     (lam (m)
                       (mol%-set-sync m combined)))))

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
           `(,to)
           (migrate branch '[] to)))

(def (detach root path)
  ;; Convert root-path into a root
  (migrate root path '[]))

(def (pull host to guest)
  ;; It's like updating combined with synchronizing
  (let* ([unifier new-root]
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
      (mol% (for/or ([sync-ls topo])
              (if (member path sync-ls)
                  sync-ls
                  #f))
            (cmol%-data cm)
            (for/list ([kid  (cmol%-kids cm)]
                       [i    (in-naturals)])
              (loop kid (rcons path i)))))))

;;; Compressed molecule using pointers
(def (cmol% data kids)
  `(,data ,@kids))

(def cmol%-data first)
(def (cmol%-set-data mol val)
  (list-set mol 0 val))

(def cmol%-kids cdr)
(def (cmol%-set-kids mol val)
  `(,(cmol%-data mol) ,@val))

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
          (loop kid (rcons path i))))))

  (let ([result  (make-hasheq)])
    (cascade-path cmol
                  (lam (cm path)
                    (hash-set! result
                               cm
                               (cons path
                                     (hash-ref! result cm null)))))
    (hash-values result)))

;;; Printing
(def (wm cmol? [port (current-output-port)])
  (parameterize ([print-graph  #t])
    (display (match cmol?
               [(list _ _ _)  (compress cmol?)]
               [(list _ _)    cmol?]  #|Compress if necessary|#)
             port)))

(def (dm cmol? [port (current-output-port)])
  ;; The difference is the column restriction
  (parameterize ([print-graph           #t]
                 [pretty-print-columns  35])
    (pdisplay (match cmol?
                [(list _ _ _)  (compress cmol?)]
                [(list _ _)    cmol?])
              35
              port)))

;;; Assignment (NOT mutation)
(define-syntax-rule (update! iden more ...)
  (set! iden (update iden more ...)))

(define-syntax-rule (sync! iden more ...)
  (set! iden (sync iden more ...)))

(define-syntax-rule (pull! iden more ...)
  (set! iden (pull iden more ...)))
