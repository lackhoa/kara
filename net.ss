(define treeo
  (lambda (tree value children)
    (== `(,value . ,children) tree)))

(define hier
  ;; This is a forest
  '((*p (tcp (telnet) (www))
        (udp (dns))
        (icmp (echo) (echo-reply)))
    (*a
     (pub-net (isp))
     (priv-net (ten-net (vlan1 (pc-a))
                        (vlan2 (pc-c)))))
    (*e (#t))))

(define tree-findt
  (lambda (tree sub path)
    (fresht (v _f)
      (wrapt (== `(,v . ,_f) sub))
      (;; Mutual tree recursion
       letrec ([tree-findt
                (lambda (tree path^)
                  (lambda (found?)
                    (fresh (u children)
                      (treeo tree u children)
                      (condo
                       [;; The only case we "return" path
                        (==t u v) (== #t found?)
                        (reverseo path^ path) (== tree sub)]
                       [else
                        ((forest-findt children `(,u . ,path^)) found?)]))))]
               [forest-findt
                (lambda (forest path^)
                  (lambda (found?)
                    (conde
                     [(== '() forest) (== #f found?)]
                     [(fresh (a d)
                        (== `(,a . ,d) forest)
                        (condo
                         [(tree-findt a path^) (== #t found?)]
                         [else ((forest-findt d path^) found?)]))])))])
        (tree-findt tree '())))))

(define forest-findt
  (lambda (forest sub path)
    (tree-findt `(dummy . ,forest) sub `(dummy . ,path))))

(define reverseo
  (lambda (l1 l2)
    (let reverseo ([l1 l1] [l '()])
      (conde
       [(== '() l1) (== l l2)]
       [(fresh (a d l+)
          (== `(,a . ,d) l1)
          (== `(,a . ,l) l+)
          (reverseo d l+))]))))

(define supert
  ;; If v1 is not found in the hierarchy -> v2 must be same
  (lambda (v1 v2)
    (disjt
     (==t v1 v2)
     (fresht (f1 _p1 _p2 _f2)
       (forest-findt hier `(,v1 . ,f1) _p1)
       (forest-findt f1 `(,v2 . ,_f2) _p2)))))
(define super*t
  (lambda (e1 e2)
    (lambda (?)
      (conde
       [(== '() e1) (== '() e2) (== #t ?)]
       [(fresh (a1 d1 a2 d2)
          (== `(,a1 . ,d1) e1)
          (== `(,a2 . ,d2) e2)
          ((conjt (supert a1 a2)
                  (super*t d1 d2))
           ?))]))))

;; Packet = (protocol, source-ip, destination-ip, est?)
;; An entry is just a packet with an action attached
(define entry-matcht
  (lambda (entry pkt)
    (lambda (?)
      (fresh (eaction epkt)
        (== `(,eaction . ,epkt) entry)
        ((super*t epkt pkt) ?)))))

(define acl-map
  (lambda (acl pkt entry)
    (conde
     [(== '() acl)
      (== '(deny * * * *) entry)]
     [(fresh (a d)
        (== `(,a . ,d) acl)
        (condo
         [(entry-matcht a pkt) (== a entry)]
         [else (acl-map d pkt entry)]))])))
