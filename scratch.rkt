(let-values ([(in out) (make-pipe)])
  (parameterize ([print-graph #t])
    (write cmol out))
  (read in))

;;; Batch jobs
(let ([flag  (command-line #:args ([flag "0"]) flag)])
  (case flag
    [("0")      (void)]

    [("1" "2")  (let ([FILE-NAME  (match flag
                                    ["1"  "db/data1"]
                                    ["2"  "db/data2"])])
                  (load FILE-NAME)
                  (for ([i  (in-naturals)])
                    (pydisplay "Cycle number:" i)
                    (time (com 10))
                    (time (col 1))
                    (displayln (num))
                    (save FILE-NAME)))]))

(def (same? root path1 path2)
  ;; mol% -> path -> path -> bool
  ;; the meaning of uttering "path1 is synced with path2"
  (let ([mol1  (ref root path1)]
        [mol2  (ref root path2)])
    (match (andb mol1 mol2)
      [#t  (orb (member path1 (mol%-sync mol2))
                (let ([ctor  (mol%-data mol1)])
                  (match (mol%-data mol2)
                    [#f        #f]
                    [(== ctor)  (let ([kpaths1  (kids-paths root path1)]
                                     [kpaths2  (kids-paths root path2)])
                                 (andb (eq? (ctor-arity ctor)
                                            (length kpaths1)
                                            (length kpaths2))
                                       (for/andb ([kp1  kpaths1]
                                                  [kp2  kpaths2])
                                         (same? root kp1 kp2))))]
                    [_         #f])))]
      [#f  (andb (not (orb mol1 mol2)
                    #|If they were synced, both would exist|#)
                 (same? root
                        (rcdr path1)
                        (rcdr path2)))])))



(def (instance? ins model)
  ;; mol% -> mol% -> bool
  ;; Check if `ins` is an instance? of `model`
  (def (same? root path1 path2)
    ;; mol% -> path -> path -> bool
    ;; the meaning of uttering "path1 is synced with path2"
    (def (same-down? mol1 mol2 path1 path2)
      (orb (member path1 (mol%-sync mol2)  #|explicitly synced|#)
           (let ([ctor  (mol%-data mol1)])
             (match (mol%-data mol2)
               [#f        #f]
               [(== ctor)  (let* ([kids1  (mol%-kids mol1)]
                                  [kids2  (mol%-kids mol2)]
                                  [klen   (length kids1)])
                             (andb (eq? klen
                                        (ctor-arity ctor)
                                        (length kids2))
                                   (for/andb ([kid1  kids1]
                                              [kid2  kids2]
                                              [i     (in-range klen)])
                                     (same-down? kid1 kid2
                                                 `(,@path1 ,i)
                                                 `(,@path2 ,i)))))]
               [_         #f]))))
    (trace same-down?)

    (let-values ([(post p1 p2)
                  (split-common-postfix path1 path2)])
      (let loop ([posti  post  #|Tracing the path down|#]
                 [p1i    p1]
                 [p2i    p2]
                 [mol1   (ref root p1)]
                 [mol2   (ref root p2)])
        (displayln "This is mol1")(displayln mol1)
        (andb (andb mol1 mol2  #|Both do exist|#)
              (match posti
                ['()              (same-down? mol1 mol2 p1i p2i)]
                [`(,pcar ,@pcdr)  (let ([kid1  (ref mol1 `[,pcar])]
                                        [kid2  (ref mol2 `[,pcar])])
                                    (orb (andb (not (orb kid1 kid2)  #|Both do not exist|#)
                                               (member p1i (mol%-sync mol2)  #|explicit sync|#))
                                         (loop pcdr
                                               kid1
                                               kid2
                                               `(,@p1i ,pcar)
                                               `(,@p2i ,pcar))))])))))

  (let ([cache  '()  #|The cache, in the sync list is long|#])
    (let loop ([path  '[]]
               [mol   model])
      (andb (match (mol%-data mol)
              [#f  #t]
              [md  (eq? md (ref-data ins path))]  #|Data|#)

            (let* ([sync-ls  (mol%-sync mol)]
                   [pct      (car sync-ls)])
              (orb (findf (lam (ci)  (member pct ci))
                          cache)
                   (andb (for/andb ([p  (cdr sync-ls)])
                           (same? ins pct p))
                         (cons! sync-ls cache))) #|Topology|#)

            (let ([kids  (mol%-kids mol)])
              (for/andb ([kid-path  (for/list ([i  (range (length kids))])
                                      (rcons path i))]
                         [kid       kids])
                (loop kid-path kid)  #|Recursion|#))))))

(def (msync root path1 path2)
  ;; Establish a new synchronization, update if needed.
  (def (sync-data root p1 p2)
    (match* ((ref-data root p1)
             (ref-data root p2))
      [(x x)    root]
      [(#f d2)  (update root p1 d2)]
      [(d1 #f)  (update root p2 d1)]
      [(_ _)    #f]))

  (def (level-kids root p1 p2)
    (let ([i1  (last-index (ref-kids root p1))]
          [i2  (last-index (ref-kids root p2))])
      ;; Add the missing kids
      (cond [(< i1 i2)  (update root (rcons p1 i2))]
            [(> i1 i2)  (update root (rcons p2 i1))]
            [else       root])))

  (def (sync-sync root p1 p2)
    (def (incest? p1 p2  #|Naming much?|#)
      (let-values ([(tl1 tl2)
                    (drop-common-prefix p1 p2)])
        (or (null? tl1) (null? tl2))))

    (let ([sy1  (ref-sync root p1)]
          [sy2  (ref-sync root p2)])
      (and (not (for*/or ([p1  sy1]
                      [p2  sy2])
              (incest? p1 p2)))

         (let ([proc  (f> mol%-set-sync (append sy1 sy2))])
           #|Establish the connections|#
           (do&inform (do&inform root p1 proc) p2 proc)))))

  (let* ([root        (update root path1)]
         [root        (update root path2)])
    (let loop ([root  root]
               [p1    path1]
               [p2    path2])
      (match (bool (member p1 (ref-sync root p2)))
        [#t  root]
        [#f  (>> root
                 (f> sync-data  p1 p2)
                 (f> level-kids p1 p2)
                 (f> sync-sync  p1 p2)
                 (lam (r) (for/fold ([r  r]) ([new-p1  (kids-paths r p1)]
                                            [new-p2  (kids-paths r p2)])
                          #:break (not r)
                          (loop r new-p1 new-p2))))]))))
