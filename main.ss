(load "mini-kanren/mk.scm")
(load "mini-kanren/numbers.scm")

(load "kara-lang/main.ss")
(load "rels.ss")
(load "bfs.ss")
(load "cate.ss")

(define pp pretty-print)
(define lpp (l> for-each pp))

(lpp
 (run 6 (s)
   (search s)))
