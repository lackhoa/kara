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

(define acl100
  '([deny   tcp *        * * * *]
    [permit *   good-adr * * * *]
    [permit www ok-adr   * * * 1]))

(define hier '([ip tcp udp icmp]
               [tcp telnet www]
               [udp dns]))

(define direct-connections
  '([pc-a   . r1-g]
    [r1-g   . r1-s]
    [r1-s   . isp-s0]
    [isp-s0 . isp-s1]
    [isp-s1 . r3-s]
    [r3-s   . r3-g]
    [r3-g   . pc-c]))

(define address-table
  `([pc-a . 10] [pc-c . 30]))

;; (load "net.ss")
(load "stick.ss")

(lpp
 (run 10 (o)
   (shapes-shift-overlaps pyramid1 pyramid2 o)))
