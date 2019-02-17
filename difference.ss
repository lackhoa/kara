(define read-entire-file
  (lambda (ip)
    (let loop ([next (read ip)]
               [res '()])
      (if (eq? next '#!eof) (reverse res)
          (loop (read ip) (cons next res))))))

(define get-difference
  (lambda (l1 l2)
    (let loop ([l1 l1]
               [l2 l2]
               [res1 '()])
      (cond
       [(null? l1) (list res1 l2)]
       [(null? l2) (list (append l1 res) res2)]
       [(member (car l1) l2)
        (loop (cdr l1) (remove (car l1) l2) res1)]
       [else
        (loop (cdr l1) l2 (cons (car l1) res1))]))))
(define pp (lambda (ls) (for-each pretty-print ls)))
;; 2 is mine
(display
 (cadr (get-difference (read-entire-file (open-input-file "text1"))
                       (read-entire-file (open-input-file "text2")))))
