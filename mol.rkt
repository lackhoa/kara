#lang racket
(require "lang/kara.rkt"
         racket/hash
         racket/struct)
(provide (all-defined-out))

;;; Molcules
;; var% =  int
;; mol% =  ctor: symbol, kids: [var% | mol%]

(def new-var '(var% 0))

(def (mdispatch mol/var fv fm)
  (match mol/var
    [`(var% ,v)            (fv v)]
    [`(mol% ,ctor ,@kids)  (fm ctor kids)]))

(def (ref mol/var path)
  ;; Fault-tolerant referencing
  (match path
    [(list)            mol/var]
    [`(,next ,@rest)  (let ([kids  (mdispatch mol/var
                                              (lam (v)  #f)
                                              (lam (_ kids)  kids))])
                        (and kids
                           (< next (length kids))
                           (ref (list-ref kids next)
                                rest)))]))

(def (replace mol/var v new)
  ;; Crucial auxiliary function
  (mdispatch mol/var
             (lam (u)
               (cond [(eq? u v)  new]
                     [else       mol/var]))
             (lam (ctor kids)
               `(mol% ,ctor
                      ,@(for/list ([kid  kids])
                          (replace kid v new))))))

(def (has-var? mol/var var)
  (mdispatch mol/var
             (f> eq? var)
             (lam (_ kids)
               (for/or ([kid  kids])
                 (has-var? kid var)))))

(def (msync mol path1 path2)
  (let inner ([m  mol]
              [p  '[]])
    (match* ((ref m `(,@path1 ,@p))
             (ref m `(,@path2 ,@p)))
      [(m/v1 `(var% ,v2))
       (and (not (has-var? m/v1 v2)  #|No Incest|#)
          (replace m v2 m/v1))]

      [(`(var% ,v1) m/v2)
       (and (not (has-var? m/v2 v1)  #|No Incest!|#)
          (replace m v1 m/v2))]

      [(`(mol% ,ctor1 ,@kids1)
        `(mol% ,ctor2 ,@kids2))
       (and (eq? ctor1 ctor2)
          (let ([new-paths  (for/list ([i  (range (length kids1))])
                              `(,@p ,i))])
            (let inner2 ([m   m]
                         [ps  new-paths])
              (match ps
                ['()               m]
                [`(,new-p ,@rest) (>> (inner m new-p)
                                      (f> inner2 rest))]))))])))

(def (last-var mol/var)
  (mdispatch mol/var
             identity
             (lam (_ kids)
               (match kids
                 ['()  0]
                 [_    (apply max (map last-var kids))]))))

(def (translate mol/var n)
  (mdispatch mol/var
             (lam (v)  `(var% ,(+ v n)))
             (lam (ctor kids)
               `(mol% ,ctor
                      ,@(map (f> translate n) kids)))))

(def (up host to guest)
  (let* ([unifier  `(mol% #f
                          ,host
                          ,(translate guest
                                      (add1 (last-var host))))
                   #|Host as 1st kid|#
                   #|Guest as 2nd kid|#])
    (>> (msync unifier
               `[0 ,@to]
               '[1])
        (f> ref '[0]  #|That's the new host|#))))

;; (def (dm cmol? [port (current-output-port)])
;;   ;; The difference is the column restriction
;;   (pdisplay (match cmol?
;;               [(list _ _ _)  (compress cmol?)]
;;               [(list _ _)    cmol?])
;;             35
;;             port))
