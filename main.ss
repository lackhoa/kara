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
;; (load "stick.ss")
(load "reif.ss")
;; (load "full-interp.ss")
(load "faster-miniKanren/full-interp.scm")

(display (time (run 500 (_.0 _.1 _.2 _.3)
                 (evalo
                  `(match ,_.0 [`,_.0 ,_.1] . ,_.2)
                  _.1))))
