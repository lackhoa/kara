(load "mini-kanren/mk.scm")
(load "mini-kanren/numbers.scm")

(load "kara-lang/main.ss")
(load "rels.ss")
(load "bfs.ss")
(load "chain.ss")

(define query-display
  (l> for-each (lambda (x) (display x) (newline))))

;; (query-display
;;  (run* (x y)
;;    (rembero x '[0 1 0 4 5] y)))

(query-display
 (arcs '((-) (0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))))

;; (display (sort (lambda (x y)
;;                  (or (equal? x '[-]) (< (length x) (length y))))
;;                '((-) (0 0) (0) (0 0 0 0) (0 0 0 0) (0 0 0 0))))
