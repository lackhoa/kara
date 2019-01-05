(load "mini-kanren/mk.scm")
(load "mini-kanren/numbers.scm")

(load "kara-lang/main.ss")
(load "rels.ss")
(load "bfs.ss")
(load "zurg.ss")

(define qpp
  (l> for-each (lambda (x) (pretty-print x) (newline))))

(qpp
 (run 3 (s)
   (main s)))
