(import (kara-lang main))

;; Molcules:
;; var% =  int
;; mol% =  ctor: symbol, kids: [var% | mol%]
(define new-var 0)

(define (mol-< mol/var fv fm)
  ;; Dispatch function for molecule
  (if (number? mol/var)
      (fv mol/var)
      (fm (car mol/var) (cdr mol/var))))

(define (ref mol/var path)
  ;; Fault-tolerant referencing
  (if (null? path)  mol/var
      (mol-< mol/var
             (lambda (v)       #f  #|can't go down|#)
             (lambda (_ kids)  (and (< (car path) (length kids))
                             (ref (list-ref kids (car path))
                                  (cdr path)))))))

(define (has-var? mol/var var)
  (mol-< mol/var
         (f> eq? var)
         (lambda (_ kids)
           (exists (f> has-var? var) kids))))

(define (sync mol path1 path2)
  (let inner ([m  mol]
              [p  '[]])
    (let ([m/v1  (ref m `[,@path1 ,@p])]
          [m/v2  (ref m `[,@path2 ,@p])])
      (mol-< m/v1
             (lambda (v1)
               (and (mol-< m/v2
                         (lambda _ #t)
                         (lambda _ (not (has-var? m/v2 v1)  #|No Incest!|#)))
                  (substq m/v2 v1 m  #|v1 to be replaced by m/v2|#)))
             (lambda (ctor1 kids1)
               (mol-< m/v2
                      (lambda (v2)
                        (and (not (has-var? m/v1 v2)  #|No Incest|#)
                           (substq m/v1 v2 m)))
                      (lambda (ctor2 kids2)
                        (and (eq? ctor1 ctor2  #|constructor|#)
                           (= (length kids1) (length kids2)  #|arity|#)
                           (let ([new-paths  (map (lambda (i) `[,@p ,i])
                                                  (enumerate kids1))])
                             (do ([ps new-paths (cdr ps)]
                                  [m  m         (inner m (car ps))])

                                 ((or (not m) (null? ps))  m)))))))))))

(define last-var
  (f> mol-<
      (lambda (v) v)
      (lambda (_ kids)
        (cond [(null? kids)  0]
              [else          (apply max (map last-var kids))]))))

(define translate
  (lambda (mol n)
    (mol-< mol
           (l> + n)
           (lambda (ctor kids)
             (cons ctor
                   (map (f> translate n) kids))))))

(define (up host to guest)
  (let ([unifier  `(f  #|Dummy constructor|#
                    ,host
                    ,(translate guest
                                (add1 (last-var host))))
                  #|Host as 1st kid|#
                  #|Guest as 2nd kid|#])
    (>> (sync unifier
              `[0 ,@to]
              '[1])
        (f> ref '[0]  #|That's the new host|#))))

(define (clean mol/var)
  (let ([dic      (make-eq-hashtable)]
        [counter  -1]  #|State variables|#)
    (let inner ([m/v  mol/var])
      (mol-< m/v
             (lambda (v)
               (let ([lookup  (eq-hashtable-ref dic v #f)])
                 (case lookup
                   [#f    (set! counter (+ counter 1))
                          (eq-hashtable-set! dic v counter)
                          counter]
                   [else  lookup])))
             (lambda (ctor kids)
               `(,ctor
                 ,@(map (lambda (kid)
                          (inner kid  #|Influenced by `dic` and `counter`|#))
                        kids)))))))
