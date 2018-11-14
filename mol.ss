(import (kara-lang main))

;; Molcules:
(define new-var 0)

(define mol-<
  ;; Dispatch function for molecule
  (lambda (mol fv fc fl)
    ((cond [(number? mol)  fv]
           [(atom? mol)    fc]
           [(list? mol)    fl]) mol)))

(define ref
  ;; Fault-tolerant referencing
  (lambda (mol path)
    (if (null? path)  mol
        (and (list? mol)
           (and (< (car path) (length mol))
              (ref (list-ref mol (car path))
                   (cdr path)))))))

(define has-var?
  (lambda (mol var)
    (mol-< mol
           (lambda (v) (eq? v var))
           (lambda (c) #f)
           (lambda (ls)
             (exists (f> has-var? var) ls)))))

(define sync
  (lambda (mol path1 path2)
    (let loop ([m  mol]
               [p  '[]])
      (let ([m1  (ref m `[,@path1 ,@p])]
            [m2  (ref m `[,@path2 ,@p])])
        (mol-< m1
               (#|m1 is a variable|#
                lambda (v1)
                 (mol-< m2
                        (lambda (v2)  (substq v2 v1 m))
                        (lambda (c2)  (substq c2 v1 m))
                        (lambda (l2)  (and (not (has-var? l2 v1)  #|No Incest!|#)
                                    (substq l2 v1 m)))))
               (#|m1 is a constant|#
                lambda (c1)
                 (mol-< m2
                        (lambda (v2)  (substq c1 v2 m))
                        (lambda (c2)  (and (eq? c1 c2) m))
                        (lambda (l2)  #f)))
               (#|m1 is a list|#
                lambda (l1)
                 (mol-< m2
                        (lambda (v2)  (and (not (has-var? l1 v2)  #|No Incest!|#)
                                    (substq l1 v2 m)))
                        (lambda (c2)  #f)
                        (lambda (l2)  (and (= (length l1) (length l2))
                                    (let loop2 ([m   m]
                                                [ps  (#|New paths|#
                                                      map (lambda (i) `[,@p ,i])
                                                          (enumerate l1))])
                                      (cond [(not m)       #f]
                                            [(null? ps)  m]
                                            [else        (loop2 (loop m (car ps))
                                                                (cdr ps))])))))))))))

(define last-var
  (f> mol-<
      (lambda (v) v) (lambda (c) 0)
      (lambda (ls)
        (cond [(null? ls)  0]
              [else        (apply max (map last-var ls))]))))

(define translate
  (lambda (mol n)
    (mol-< mol
           (lambda (v) (+ v n))  (lambda (c) c)
           (lambda (ls)
             (map (f> translate n) ls)))))

(define up
  (lambda (host to guest)
    (let ([unifier  `(,host  #|Host as first item|#
                      ,(translate guest
                                  (add1 (last-var host))))
                    #|Guest as second item|#])
      (>> (sync unifier
                `[0 ,@to]
                '[1])
          (f> ref '[0]  #|returns the new host|#)
          clean))))

(define clean
  (lambda (mol)
    (let ([var-handler  #|The variable generator and bookkeepers|#
           (let ([dic      (make-eq-hashtable)]
                 [counter  -1])
             (lambda (v)
               (let ([lookup  (eq-hashtable-ref dic v #f)])
                 (case lookup
                   [#f    (set! counter (+ counter 1))
                          (eq-hashtable-set! dic v counter)
                          counter]
                   [else  lookup]))))])

      (#|This loop is influenced by var-handout|#
       let loop ([m  mol])
        (mol-< m
               (lambda (v)  (var-handler v))
               (lambda (c)  c)
               (lambda (ls) (map loop ls)))))))
