;;; Example data
(define acl100
  '([permit www        ten-net *a         *e]
    [permit tcp        pc-a    r3-s       *e]
    [permit *p         ten-net twenty-net *e]
    [permit echo       ten-net twenty-net *e]
    [permit echo-reply ten-net twenty-net *e]))

(define acl101
  '([permit www ok-*a   *a #t]
    [deny   tcp *a      *a *e]
    [permit *p  good-*a *a *e]))

(define web-policy
  '([permit www thirty-net r1-s    *e]
    [permit www thirty-net isp-net *e]))



;;; T*es
;;; Tree t*es
(pp "Fuck you")
(pp (run* (p found?) (fresh (f u) ((forest-findt hier u p) found?))))
#!eof

(pp "tree-findt vlan1: found")
(pp (run* (p found?) (fresh (f) ((forest-findt hier `(vlan1 . ,f) p) found?))))

(pp "tree-findt vlan3: not found")
(pp (run* (p found?) (fresh (f) ((forest-findt hier `(vlan3 . ,f) p) found?))))

(pp "tree-findt fun (found)")
(pp (run* (t p) ((forest-findt hier t p) #t)))

(pp "tree-findt fun 2 (not found)")
(pp (run* (t p) ((forest-findt hier t p) #f)))

(pp "tree-refo")
(pp (run* (p q) (tree-refo hier p q)))


(pp "supert")
(pp (run* (v1 v2) ((supert v1 v2) #t)))

(pp "super*t")
(pp (run 5 (v2) ((super*t '(*p *a *a *e) v2) #t)))

;;; ACL t*es
(pp "acl101")
(pp (run 10 (pkt entry)
      (acl-map acl101 pkt entry)))

(pp "acl100")
(pp (run 10 (pkt entry)
      (acl-map acl100 pkt entry)))

#!eof
