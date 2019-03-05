(define hier
  ;; This is a tree
  '(* (tcp (telnet) (www))
      (udp (dns))
      (icmp (echo) (echo-reply))
      (ten-net (vlan1 (pc-a))
               (vlan2 (pc-c)))))

(define treeo
  (lambda (value children tree)
    (== `(,value . ,children) tree)))

(define tree-findt
  ;; Returns the tree whose root is the value
  (lambda (v tree tree-res)
    (lambda (found?)
      (;; Mutual recursion ahead!
       letrec ([tree-findt1
                (lambda (tree)
                  (lambda (found?)
                    (fresh (u children)
                      (treeo u chidlren tree)
                      (condo
                       [(==t u v) (== #t found?)]
                       [else ((tree-findt2 children) found?)]))))]
               [tree-findt2
                (lambda (forest tree)
                  ;; "tree" is the tree in the forest we're looking for
                  (lambda (found?)
                    (conde
                     [(== '() forest) (== #f found?)]
                     [(fresh (a d)
                        (== `(,a . ,d) forest)
                        (condo
                         [(tree-findt1 a) (== tree a)]
                         [else ((tree-findt2 d tree) found?)]))])))])
        ((tree-findt1 tree) found?)))))

(define superst
  (lambda (v1 v2)
    (lambda (?)
      (let superst1 ([tree hier])
        (conde
         [(symbolo tree)
          (== )]))
      (condo
       [(==t v1 v2) (== #t res)]
       [else
        (fresh (lu)
          (lookupo range lu)
          (conde
           [(== #f lu) (== #f res)]
           [(=/= #f lu) (any-coverso lu value res)]))]))))


;; Packet = (protocol, source-ip, destination-ip, est?)

(define packeto
  ;; A four-tuple
  (lambda (p)
    (fresh (protocol src-adr dst-adr est?)
      (== `(,protocol ,src-adr ,dst-adr ,est?) p))))

(define entryo
  ;; An entry is just a packet with an action attached
  (lambda (e)
    (fresh (action proto src-adr dst-adr est?)
      (== `(,action ,proto ,src-adr ,dst-adr ,est?) e))))

(define aclo
  (lambda (acl)
    (conde
     [(== '() acl)]
     [(fresh (a d)
        (== `(,a . ,d) acl)
        (entryo a)
        (aclo d))])))



(define entry-match?
  (lambda (entry pkt res)
    (fresh (eaction epkt fms)
      (== entry `(,eaction . ,epkt))
      (mapo2 coverso epkt pkt fms)
      (conde
       [(== fms '(#t #t #t #t)) (== res #t)]
       [(membero #f fms)       (== res #f)]))))
(define membero
  (lambda (mem ls)
    (fresh (a d)
      (== ls `(,a . ,d))
      (conde [(== a mem)]
             [(=/= a mem) (membero mem d)]))))
(define mapo2
  (lambda (f l1 l2 out)
    (conde [(== l1 '()) (== out '())]
           [(fresh (a1 d1 a2 d2 fa d-out)
              (== l1 `(,a1 . ,d1))
              (== l2 `(,a2 . ,d2))
              (f a1 a2 fa)
              (== out `(,fa . ,d-out))
              (mapo2 f d1 d2 d-out))])))

(define acl-map
  (lambda (acl pkt matched-entry)
    (conde [(== acl '())
            (== '(deny * * * *) matched-entry)]
           [(fresh (entry acl-rest eres)
              (== `(,entry . ,acl-rest) acl)
              (entry-match? entry pkt eres)
              (conde
               [(== #t eres)
                (== matched-entry entry)]
               [(== #f eres)
                (acl-map acl-rest pkt matched-entry)]))])))

(define acl-permit
  (lambda (acl pkt permit-entry)
    (fresh (epkt)
      (== permit-entry `(permit . ,epkt))
      (acl-map acl pkt permit-entry))))

(define acl-deny
  (lambda (acl pkt deny-entry)
    (fresh (epkt)
      (== deny-entry `(deny . ,epkt))
      (acl-map acl pkt deny-entry))))

(define acl-allow
  (lambda (acl pkt)
    (fresh (e)
      (acl-permit acl pkt e))))

(define acl-block
  (lambda (acl pkt)
    (fresh (e)
      (acl-deny acl pkt e))))
