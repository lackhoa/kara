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
