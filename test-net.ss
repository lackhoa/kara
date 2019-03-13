;;; Example data
(define acl100
  '([permit www        ten-net adr        est]
    [permit tcp        pc-a    r3-s       est]
    [permit ip         ten-net twenty-net est]
    [permit echo       ten-net twenty-net est]
    [permit echo-reply ten-net twenty-net est]))

(define acl101
  '([permit www ok-adr   adr #t]
    [deny   tcp adr      adr est]
    [permit ip  good-adr adr est]))

(define web-policy
  '([permit www thirty-net r1-s    est]
    [permit www thirty-net isp-net est]))



;;; Tests
;;; Tree tests
(pp "tree-findt vlan1: found")
(pp (run* (q found?) ((forest-findt hier 'vlan1 q) found?)))

(pp "tree-findt vlan3: not found")
(pp (run* (q found?) ((forest-findt hier 'vlan3 q) found?)))

(pp "tree-findt fun (found)")
(pp (run* (q) (fresh (v) ((forest-findt hier v q) #t))))

(pp "tree-findt fun 2 (not found)")
(pp (run* (v) (fresh (t) ((forest-findt hier v t) #f))))

(pp "supert")
(pp (run* (v1 v2) ((supert v1 v2) #t)))

(pp "super*t")
(pp (run 5 (v2) ((super*t '(ip adr adr est) v2) #t)))

;;; ACL tests
(pp "acl101")
(pp (run 10 (pkt entry)
      (acl-map acl101 pkt entry)))

(pp "acl100")
(pp (run 10 (pkt entry)
      (acl-map acl100 pkt entry)))

#!eof
