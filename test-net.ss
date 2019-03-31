(load "net.ss")

;;; Example data
(define hier
  ;; This is a forest
  '((*p (tcp (telnet) (www))
        (udp (dns))
        (icmp (echo) (echo-reply)))
    (*a
     (pub-net (isp))
     (priv-net (ten-net (vlan1 (pc-a))
                        (vlan2 (pc-c)))))
    (*e (#t) (#f))))

(define acl100
  '([permit www        ten-net *a         *e]
    [permit tcp        pc-a    r3-s       *e]
    [permit *p         ten-net twenty-net *e]
    [permit echo       ten-net twenty-net *e]
    [permit echo-reply ten-net twenty-net *e]))

(define acl101
  '([permit www ok-a    *a #t]
    [deny   tcp *a      *a *e]
    [permit *p  good-a  *a *e]))

(define web-policy
  '([permit www thirty-net r1-s    *e]
    [permit www thirty-net isp-net *e]))



;;; Tests
;;; Tree tests
(pp "tree-findt vlan1: found")
(pp
 (run* (p found?)
   (fresh (f) ((forest-findt hier `(vlan1 . ,f) p) found?))))

(pp "tree-findt vlan3: not found")
(pp (run* (p found?) (fresh (f) ((forest-findt hier `(vlan3 . ,f) p) found?))))

(pp "tree-findt fun (found)")
(pp (run* (t p) ((forest-findt hier t p) #t)))

(pp "tree-findt fun 2 (not found)")
(pp (run* (t p) ((forest-findt hier t p) #f)))

(pp "supert")
(pp (run* (v1 v2) ((supert v1 v2) #t)))

(pp "super*t")
(pp (run 20 (v2) ((super*t '(*p *a *a *e) v2) #t)))

;;; ACL test
(pp "acl101")
(pp (run 10 (pkt entry)
      (acl-mapo acl101 pkt entry)))

(pp "vlan1 -> ten-net")
(pp (run* (v) (fresh (?) ((upt 'vlan1 v) ?))))

(pp "www -> tcp")
(pp (run* (v) (fresh (?) ((upt 'www v) ?))))

(pp "*p -> nothing")
(pp (run* (?) (fresh (v) ((upt '*p v) ?))))

(pp "vlan3 -> nothing")
(pp (run* (?) (fresh (v) ((upt 'vlan3 v) ?))))

(pp "pkt-upo (*p vlan3 isp #f) -> (*p vlan3 pub-net *e)")
(pp (run* (q) (pkt-upo '(*p vlan3 isp #f) q)))

(pp "Same as above, but spread out to lists")
(pp (run* (q) (pkt-up*o '(*p vlan3 isp #f) q)))

(pp "If than test ran (correctly), then thank god it did!")
(pp "Now we get to the final boss: acl101 general")
(pp (run 3 (p e) (acl-map-geno acl101 p e)))



#!eof
