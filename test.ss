(import (kara-lang main))
(load "mol.ss")

(define anti-unify
  (lambda (mol1 mol2)
    (let* ([subs-lookup!
            (let ([get-var!  (let ([next  -1])
                               (lambda _  (begin (set! next (1+ next))
                                            next)))]
                  [;; subs is a list of items of the form <v t1 t2>
                   subs     '()])
              (lambda (term1 term2)
                (let loop ([nsubs  subs])
                  (if (null? nsubs)  (let ([new-var  (get-var!)])
                                       (begin (set! subs
                                                (cons (list new-var term1 term2)
                                                      subs))
                                              new-var))
                      (let* ([s   (car nsubs)]
                             [v   (car s)]
                             [t1  (cadr s)]
                             [t2  (caddr s)])
                        (or (and (equal? t1 term1)
                              (equal? t2 term2)
                              v)
                           (loop (cdr nsubs))))))))])

      (let loop ([m1  mol1]
                 [m2  mol2])
        (cond [(equal? m1 m2)  m1]
              [(and (pair? m1)
                  (pair? m2)   (cons (loop (car m1) (car m2))
                                     (loop (cdr m1) (cdr m2))))]
              [else            (subs-lookup! m1 m2)])))))
