(library (enum)
  (export instance-file debug? instance? size height)
  (import (chezscheme) (kara-lang main)
          (mol) (types))
  (define instance-file
    (make-parameter "log/instance.rkt"))
  (define debug? (make-parameter #f))

  (define log-instance
    ;; mol% -> mol% -> void
    (lambda (ccs1 ccs2)
      (with-output-to-file (instance-file)
        (lambda ()
          (pydisplay ccs1) (pydisplay (make-string 80 #\<))
          (pydisplay ccs2) (newline)))))

  (define height
    (lambda (mol/var)
      ;; Just a metric
      (mol-< mol/var
             (lambda (_) 0)
             (lambda (_ kids)
               (add1 (apply max (map height kids)))))))

  (define size
    (lambda (mol/var)
      ;; A better metric?
      (mol-< mol/var
             (lambda (_) 1)
             (lambda (_ kids)
               (add1 (apply + (map size kids)))))))

  (define instance?
    (lambda (instance model)
      (define data-match?
        (let inner ([ins  instance]
                    [mod  model])
          (mol-< mod
                 (lambda (_) #t   #|variables can be anything|#)
                 (lambda (mod-data mod-kids)
                   (mol-< ins
                          (lambda (_) #f  #|instance is even more general|#)
                          (lambda (ins-data ins-kids)
                            (and (eq? mod-data ins-data)
                               (andmap inner
                                       ins-kids
                                       mod-kids))))))))

      (define get-topology
        ;; Returns map of variable to paths
        (lambda ()
          (hashtable-values  #|We only care about the paths|#
           (let ([ht  (make-eq-hashtable)])
             (let inner ([m/v   model]
                         [path  '[]])
               (mol-< m/v
                      (lambda (v)
                        (eq-hashtable-update! ht
                                              v
                                              (lambda (ps) `(,path ,@ps))
                                              '()))

                      (lambda (_ kids)
                        (do ([kids kids (cdr kids)]
                             [i    0    (+ i 1)])
                            ((null? kids))
                          (inner (car kids) `(,@path ,i))))))
             ht))))

      (define synced?
        (lambda (mol paths)
          ;; the meaning of saying "`paths` are synchronized"
          (apply equal*?
            (map (l> ref mol) paths))))

      (and data-match?
         (andmap (lambda (paths)
                   (synced? instance paths))
                 (vector->list (get-topology)))
         #|Done|#
         (when (debug?)
           (log-instance instance model))
         #t))))
