(load "net.ss")

;;; Example data
(define hier
  ;; This is a forest
  '((*p (tcp (telnet) (www))
        (udp (dns))
        (icmp (echo) (echo-reply)))
    (*a (pub-net  (isp-net (isp)))
        (priv-net (ten-net (vlan1 (pc-a))
                           (vlan2 (pc-c)))
                  (twenty-net (r3-s))
                  (thirty-net (r1-s))))
    (*e (#t) (#f))))

(define acl100
  '([permit www  ten-net *a         *e]
    [permit tcp  pc-a    r3-s       *e]
    [deny   echo ten-net twenty-net *e]
    [permit icmp ten-net twenty-net *e]))

(define web-policy
  '([permit www thirty-net r1-s    *e]
    [permit www thirty-net isp-net *e]))

;;; Tests
;;; Tree tests
(pp "tree-findt vlan1: found")
(pp
 (run* (found?)
   (fresh (f) ((forest-findt hier `(vlan1 . ,f)) found?))))

(pp "tree-findt vlan3: not found")
(pp (run* (found?) (fresh (f) ((forest-findt hier `(vlan3 . ,f)) found?))))

(pp "tree-findt fun (not found)")
(pp (run* (t) ((forest-findt hier t) #f)))

(pp "supert")
(pp (run* (v1 v2) ((supert v1 v2) #t)))

(pp "super*t")
(pp (run 20 (v2) ((super*t '(*p *a *a *e) v2) #t)))

;;; ACL test
(pp "acl100 on [echo pc-a r3-s #f] -> should deny")
(pp (let ([pkt '[echo pc-a r3-s #f]])
      (run* (entry)
        (acl-mapo acl100 pkt entry))))

(pp "acl100 all")
(pp (run 10 (pkt entry)
      (acl-mapo acl100 pkt entry)))

(pp "acl100 deny")
(pp (run 10 (pkt entry)
      (fresh (?) (== `(deny . ,?) entry))
      (acl-mapo acl100 pkt entry)))

(pp "acl100 permit icmp")
(pp (run 1 (pkt entry)
      (fresh (?) (== '[permit icmp ten-net twenty-net *e] entry))
      (acl-mapo acl100 pkt entry)))

(pp "acl100 hole")
(pp (run 1 (missing)
      (fresh (acl e1 e2)
        (== `([permit www  ten-net *a         *e]
             [permit tcp  pc-a    r3-s       *e]
             ,missing
             [permit icmp ten-net twenty-net *e])
           acl)
        (fresh (?) (== `(deny . ,?)   e1))
        (fresh (?) (== `(permit . ,?) e2))
        (acl-mapo acl '[echo       pc-a r3-s #f] e1)
        (acl-mapo acl '[echo-reply pc-a r3-s #f] e2))))

(pp "web-policy")
(pp (run 10 (pkt entry)
      (acl-mapo web-policy pkt entry)))

#!eof
(((echo-reply pc-a r3-s #t)
  (permit icmp ten-net twenty-net *e)))
