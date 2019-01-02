(load "mini-kanren/mk.scm")
(load "mini-kanren/numbers.scm")

(load "kara-lang/main.ss")
(load "rels.ss")
(load "bfs.ss")

(define query-display
  (l> for-each (lambda (x) (display x) (newline))))

(query-display
 (run* (q)
   (main q)))
