#lang racket
(require "lang/kara.rkt"
         racket/hash
         racket/struct)
(provide (all-defined-out))

;;; Molcules
;; var% =  int
;; mol% =  ctor: symbol, kids: [var% | mol%]

(def new-var '(var% 0))

(def (mfold mol/var fv fm)
  (match mol/var
    [`(var% ,v)            (fv v)]
    [`(mol% ,ctor ,@kids)  (fm ctor kids)]))

(def (ref mol/var path)
  ;; Fault-tolerant referencing
  (match path
    [(list)            mol/var]
    [`(,next ,@rest)  (let ([kids  (mfold mol/var
                                          (lam (v)  #f)
                                          (lam (_ kids)  kids))])
                        (and (< next (length kids))
                           (ref (list-ref kids next)
                                rest)))]))

(def (replace mol/var v new)
  ;; Crucial auxiliary function
  (mfold mol/var
         (lam (u)
           (cond [(eq? u v)  new]
                 [else       mol/var]))
         (lam (ctor kids)
           `(mol% ,ctor
                  ,@(for/list ([kid  kids])
                      (replace kid v new))))))

(def (has-var? mol/var var)
  (mfold mol/var
         (f> eq? var)
         (lam (_ kids)
           (for/orb ([kid  kids])
             (has-var? kid var)))))

(def (msync mol path1 path2)
  (let inner ([m      mol]
              [stack  `((,path1 ,path2))])
    (match stack
      ['()                  m]
      [`((,p1 ,p2) ,@rest)
       (match* ((ref m p1) (ref m p2))
         [(m/v1 `(var% ,v2))
          (and (not (has-var? m/v1 v2)  #|Incest|#)
             (>> (replace m v2 m/v1)
                 (f> inner rest)))]

         [(`(var% ,v1) m/v2)
          (and (not (has-var? m/v2 v1)  #|Incest!|#)
             (>> (replace m v1 m/v2)
                 (f> inner rest)))]

         [(`(mol% ,ctor1 ,@kids1)
           `(mol% ,ctor2 ,@kids2))
          (and (eq? ctor1 ctor2)
             (let ([new-paths  (for/list ([i  (range (length kids1))])
                                 `((,@p1 ,i) (,@p2 ,i)))])
               (inner m `(,@new-paths ,@rest)
                      #|Tail recursion!|#
                      #|Note: mol did not change|#)))])])))

(def (last-var mol/var)
  (mfold mol/var
         identity
         (lam (_ kids)
           (match kids
             ['()  0]
             [_    (apply max (map last-var kids))]))))

(def (translate mol/var n)
  (mfold mol/var
         (lam (v)  `(var% ,(+ v n)))
         (lam (ctor kids)
           `(mol% ,ctor
                  ,@(map (f> translate n) kids)))))

(def (pull host to guest)
  ;; The only way to update
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
