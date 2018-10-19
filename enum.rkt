#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "types.rkt")

(provide (all-defined-out))

(def instance-file (make-parameter "log/instance.rkt"))
(def debug? (make-parameter '#f))

(def (log-instance ccs1 ccs2)
  ;; mol% -> mol% -> void
  (with-output-to-file (instance-file)
    #:exists 'append
    (thunk
     (pdisplay ccs1 35) (displayln (make-string 80 #\<))
     (pdisplay ccs2 35) (newline))))

(def (height mol/var)
  ;; Just a metric
  (mdispatch mol/var
             (const 0)
             (lam (_ kids)
               (add1 (apply max (map height kids))))))

(def (size mol/var)
  ;; A better metric?
  (mdispatch mol/var
             (const 1)
             (lam (_ kids)
               (add1 (apply + (map size kids))))))

(def (instance? instance model)
  (def (data-match?)
    (let inner ([ins  instance]
                [mod  model])
      (mdispatch mod
                 (const #t   #|variables can be anything|#)
                 (lam (mod-data mod-kids)
                   (mdispatch ins
                              (const #f  #|not here, instance is even more general|#)
                              (lam (ins-data ins-kids)
                                (and (eq? mod-data ins-data)
                                   (for/and ([ins-kid  ins-kids]
                                             [mod-kid  mod-kids])
                                     (inner ins-kid mod-kid)))))))))

  (def (get-topology)
    ;; Returns map of variable to paths
    (hash-values  #|We only care about the paths|#
     (let inner ([res   (hasheq)]
                 [m/v   model]
                 [path  '[]])
       (mdispatch m/v
                  (lam (v)
                    (hash-set res
                              v
                              (match (hash-ref res v #f)
                                [#f  `(,path)]
                                [ps  `(,path ,@ps)])))

                  (lam (_ kids)
                    (for/fold
                        ([res res])
                        ([kid kids] [kpath (for/list ([i  (range (length kids))])
                                             `(,@path ,i))])
                      (inner res kid kpath)))))))

  (def (synced? mol paths)
    ;; the meaning of saying "`paths` are synchronized"
    (apply equal?
      (map (curry ref mol) paths)))

  (and (data-match?)
     (for/and ([paths (get-topology)])
       (synced? instance paths))
     #|Done|#
     (begin (when (debug?)
              (log-instance instance model))
            #t)))
