(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")
(load "faster-miniKanren/matche.scm")

(load "kara-lang/main.ss")
(load "rels.ss")
(load "bfs.ss")
(load "cate.ss")

(define pp pretty-print)
(define lpp (l> for-each pp))

(lpp
 (run 2 (q)
   (search q)))
