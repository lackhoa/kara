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

(lpp
 (run* (q)
   (-> '(+ x 8 (* x y z)) q)))
