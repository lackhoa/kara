(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")
(load "faster-miniKanren/matche.scm")
(load "faster-miniKanren/numbers.scm")

(load "kara-lang/main.ss")
(load "rels.ss")
(load "bfs.ss")
(load "cate.ss")

(define pp pretty-print)
(define lpp (l> for-each pp))

(define-syntax run-nore
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (take n
           (inc
            ((fresh (q) g0 g ...
                    (lambdag@ (st)
                      (let ((st (state-with-scope st nonlocal-scope)))
                        (let ((z ((reify-nore q) st)))
                          (choice z (lambda () (lambda () #f)))))))
             empty-state))))
    ((_ n (q0 q1 q ...) g0 g ...)
     (run-nore n (x)
               (fresh (q0 q1 q ...)
                 g0 g ...
                 (== `(,q0 ,q1 ,q ...) x))))))

(define reify-nore
  (lambda (x)
    (lambda (st)
      (let ((c (c-from-st st x)))
        (let ((c (cycle c)))
          (let* ((S (c->S c))
                 (D (walk* (c->D c) S))
                 (Y (walk* (c->Y c) S))
                 (N (walk* (c->N c) S))
                 (T (walk* (c->T c) S)))
            (let ((v (walk* x S)))
              (let ((R (reify-S-nore v (subst empty-subst-map
                                              nonlocal-scope))))
                (reify+ v R
                        (let ((D (remp
                                  (lambda (d)
                                    (let ((dw (walk* d S)))
                                      (anyvar? dw R)))
                                  (rem-xx-from-d c))))
                          (rem-subsumed D))
                        (remp
                         (lambda (y) (var? (walk y R)))
                         Y)
                        (remp
                         (lambda (n) (var? (walk n R)))
                         N)
                        (remp (lambda (t)
                                (anyvar? t R)) T))))))))))

(define reify-S-nore
  (lambda (v S)
    (let ((v (walk v S)))
      (cond
       (;; Do not rename variables, bind it to a fresh variable
        (var? v)
        (let ((new-v (var new-scope)))
          (subst-add S v new-v)))
       ((pair? v)
        (let ((S (reify-S-nore (car v) S)))
          (reify-S-nore (cdr v) S)))
       (else S)))))

(lpp
 (run 2 (q)
   (search q)))
