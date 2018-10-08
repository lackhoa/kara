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

(def mol%-sync first)
(def mol%-data second)
(def mol%-kids third)

(def ref
  (compose first
     (while (compose notnull? 2nd)
       (pam (compose list-ref (pam (compose mol%-kids 1st)
                             (compose car 2nd)))
            (compose cdr 2nd)))
     #|(mol path)|#))

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

<<<<<<< HEAD
(def (sync root p1 p2)
;; Establish a new synchronization, expand if needed.
(begin
  #|Make sure the paths exist|#
  #|Can be replaced by let, but even better, compose|#
  (set! root (update root p1))
  (set! root (update root p2)))

=======
(def (sync root path1 path2)
  ;; Establish a new synchronization, update if needed.
  >>>>>>> 1ebda50d84702fa0b94beb3b7b34f522af7f5f86
  (def max-height
    #|watch the height to detect cycle|#
    (max (height (ref root path1))
         (height (ref root path2))))

  (def (sync-data root p1 p2 m1 m2)
    (match* ((mol%-data m1)
             (mol%-data m2))
      [(x x)    root]
      [(#f d2)  (update root p1 d2)]
      [(d1 #f)  (update root p2 d1)]
      [(_ _)    #f]))

  (def (level-kids root p1 p2 m1 m2)
    (let ([i1  (last-index (mol%-kids m1))]
          [i2  (last-index (mol%-kids m1))])
      ;; Add the missing kids
      (cond [(< i1 i2)  (update root (rcons p1 i2))]
            [(> i1 i2)  (update root (rcons p2 i1))]
            [else       root])))

  (def (sync-sync root p1 p2 m1 m2)
    (let* ([combined  (set-union (mol%-sync m1)
                                 (mol%-sync m2))]
           [proc      (lam (m)
                        (mol%-set-sync m combined))])
      #|Establish the connections|#
      (do&inform (do&inform root p1 proc) p2 proc)))

  (let loop ([root (update (update root p1) p2)]
             [p1   path1]
             [p2   path2]
             [m1   (ref root p1)]
             [m2   (ref root p2)])
    (match (bool (member fp1 (ref-sync root fp2)))
      [#t  root]
      [#f  (>> root
               (lam (r) (sync-data r p1 p2 m1 m2))
               (lam (r) (level-kids r p1 p2 m1 m2))
               (lam (r) (> (max (height (ref r path1))
                                (height (ref r path2)))
                           max-height))
               (lam (r) (sync-sync r p1 p2 m1 m2))
               (lam (r)
                 (for/fold ([r  r]) ([new-p1  (kids-paths r p1)]
                                     [new-p2  (kids-paths r p2)]
                                     [new-m1  (mol%-kids m1)]
                                     [new-m2  (mol%-kids m2)])
                           #:break (not r)
                           (loop r
                                 new-p1 new-p2
                                 new-m1 new-m2))))])))

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
