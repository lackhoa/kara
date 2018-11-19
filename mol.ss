(import (kara-lang main))

;; Molcules:
(define new-var 0)

(define mol-<
  ;; Dispatch function for molecule
  (lambda (mol fv fc fp)
    ((cond [(number? mol)  fv  #|variable|#]
           [(atom? mol)    fc  #|constant (including null)|#]
           [(pair? mol)    fp  #|pair|#]
           [else
            (error "mol-<" "Not a molecule" mol)])
     mol)))

(define ref
  ;; Fault-tolerant referencing, using cars and cdrs
  (lambda (mol path)
    (if (null? path)  mol
        (and (pair? mol)
           (ref (case (car path)
                  [car  (car mol)]
                  [cdr  (cdr mol)]
                  [else  (error "ref" "Invalid path" path)])
                (cdr path))))))

(define has-var?
  (lambda (mol var)
    (mol-< mol
           (lambda (v) (eq? v var))
           (lambda (c) #f)
           (lambda (p)
             (or (eq? (car p) var)
                (has-var? (cdr p) var))))))

(define sync
  (lambda (root path1 path2)
    (let loop ([root  root]
               [rel   '[]])
      (let ([m1  (ref root `[,@path1 ,@rel])]
            [m2  (ref root `[,@path2 ,@rel])])
        (mol-< m1
               (#|m1 is a variable|#
                lambda (v1)
                 (mol-< m2
                        (lambda (v2)  (substq v2 v1 root))
                        (lambda (c2)  (substq c2 v1 root))
                        (lambda (p2)  (and (not (has-var? p2 v1)  #|No Incest!|#)
                                    (substq p2 v1 root)))))
               (#|m1 is a constant (including null)|#
                lambda (c1)
                 (mol-< m2
                        (lambda (v2)  (substq c1 v2 root))
                        (lambda (c2)  (and (eq? c1 c2) root))
                        (lambda (p2)  #f)))
               (#|m1 is a pair|#
                lambda (p1)
                 (mol-< m2
                        (lambda (v2)  (and (not (has-var? p1 v2)  #|No Incest!|#)
                                    (substq p1 v2 root)))
                        (lambda (c2)  #f)
                        (lambda (p2)  (>> (loop root `[,@rel car])
                                     (f> loop `[,@rel cdr]))))))))))

(define last-var
  (f> mol-<
      (lambda (v)  v)
      (lambda (c)  0)
      (lambda (p)  (max (last-var (car p))
                   (last-var (cdr p))))))

(define translate
  (lambda (mol n)
    (mol-< mol
           (lambda (v) (+ v n))  (lambda (c) c)
           (lambda (p)
             (cons (translate (car p) n)
                   (translate (cdr p) n))))))

(define up
  (lambda (host to guest)
    (let ([unifier  `(,host  #|Host as first item|#
                      ,(translate guest
                                  (add1 (last-var host))))
                    #|Guest as second item|#])
      (>> (sync unifier
                `[car ,@to]
                '[cdr car])
          (f> ref '[car]  #|returns the new host|#)
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

      (#|This loop is influenced by var-handler|#
       let loop ([m  mol])
        (mol-< m
               (lambda (v)  (var-handler v))
               (lambda (c)  c)
               (lambda (p)  (cons (loop (car p))
                             (loop (cdr p)))))))))

(define-syntax bind
  (syntax-rules ()
    [(bind (id ...) exp ...)
     (let-values ([(id ...)
                   (apply values (iota (length '(id ...))))])
       exp ...)]))
