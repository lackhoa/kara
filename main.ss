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

(define acl100
  '([deny   tcp *        * * telnet  *]
    [permit *   good-adr * * *       *]
    [permit udp ok-adr   * * www     1]))

(lpp
 (run 1 (acl)
   (lengtho acl (build-num 2))
   (acl-allow acl '(10 * 20 * * *))
   (acl-block acl '(10 * 30 * * *))
   (acl-allow acl '(10 * pc-c * * *))
   ))
