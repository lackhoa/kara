;;; Example data

(define acl100
  '([permit www        ten-net *          *]
    [permit tcp        pc-a    r3-s       *]
    [permit *          ten-net twenty-net *]
    [permit echo       ten-net twenty-net *]
    [permit echo-reply ten-net twenty-net *]))

(define acl101
  '([permit www ok-adr   * #t]
    [deny   tcp *        * *]
    [permit *   good-adr * *]))

(define web-policy
  '([permit www thirty-net r1-s    *]
    [permit www thirty-net isp-net *]))

(pp "acl101 permit")
(pp (run 10 (pkt entry)
      (acl-permit acl101 pkt entry)))

(pp "acl101 deny")
(pp (run 10 (pkt entry)
      (acl-deny acl101 pkt entry)))

(pp "any-covero fun")
(pp (run))

#!eof
