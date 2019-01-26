(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")
(load "faster-miniKanren/matche.scm")
(load "faster-miniKanren/numbers.scm")

(load "kara-lang/main.ss")
(load "rels.ss")
(load "bfs.ss")
(load "net.ss")

(define pp pretty-print)
(define lpp (l> for-each pp))

(lpp
 (run 10 (p e)
   (packet? p)
   (acl-allow acl100 p e)))
