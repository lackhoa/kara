(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")
(load "faster-miniKanren/matche.scm")
(load "faster-miniKanren/numbers.scm")
;; (load "mini-kanren/mk.scm")

(load "kara-lang/main.ss")
(load "rels.ss")
(load "bfs.ss")

(define pp pretty-print)
(define lpp (l> for-each pp))

;; (load "net.ss")
(load "stick.ss")

(lpp
 (run 8 (o)
   (shapes-shift-overlaps pyramid1 pyramid2 o)))
