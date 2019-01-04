(load "mini-kanren/mk.scm")
(load "mini-kanren/numbers.scm")

(load "kara-lang/main.ss")
(load "rels.ss")
(load "bfs.ss")
(load "chain.ss")

(define query-display
  (l> for-each (lambda (x) (display x) (newline))))

(query-display
 (run 2 (s)
   (main s)))

;; (query-display
;;  (arcs '(1 (0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))))

;; (display (sort (lambda (x y)
;;                  (or (equal? x '[-]) (< (length x) (length y))))
;;                '((-) (0 0) (0) (0 0 0 0) (0 0 0 0) (0 0 0 0))))
