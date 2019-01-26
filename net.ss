(define acl100
  '([deny   tcp good-adr * * telnet  *]
    [permit *   good-adr * * *       *]
    [permit tcp ok-adr   * * 80      1]))

(define mapo2
  (lambda (f l1 l2 out)
    (conde [(== l1 '()) (== out '())]
           [(fresh (a1 d1 a2 d2 fa d-out)
              (== l1 `(,a1 . ,d1))
              (== l2 `(,a2 . ,d2))
              (f a1 a2 fa)
              (== out `(,fa . ,d-out))
              (mapo2 f d1 d2 d-out))])))

(define not-membero
  (lambda (x ls)
    (conde [(== ls '())]
           [(fresh (a d)
              (== ls `(,a . ,d))
              (=/= x a)
              (not-membero x d))])))

(define packet?
  (lambda (p)
    (fresh (protocol src-adr src-port dst-adr dst-port flags)
      (== p `(,protocol ,src-adr ,src-port ,dst-adr ,dst-port ,flags))
      (;; "*" is reserved for epackets
       not-membero '* p))))

(define entry?
  ;; An entry is just a epacket with an action attached
  (lambda (e)
    (fresh (action proto src-adr dst-adr dst-port flags)
      (== e `(,action ,proto ,src-adr ,dst-adr ,dst-port ,flags)))))

(define acl?
  (lambda (acl)
    (conde [(== acl '())]
           [(fresh (a d)
              (== acl `(,a . ,d))
              (entry? a) (acl? d))])))

(define field-match?
  (lambda (field value res)
    (conde [(== field '*) (== res 'T)]
           [(=/= field '*)
            (conde [(== field value) (== res 'T)]
                   [(=/= field value) (== res 'F)])])))

(define entry-match?
  (lambda (entry pkt res)
    ;; ACL entries: (deny/permit proto src-adr dst-adr dst-port flags)
    (fresh (eaction epkt fms)
      (== entry `(,eaction . ,epkt))
      (mapo2 field-match? epkt pkt fms)
      (conde [(== fms '(T T T T T T)) (== res 'T)]
             [(membero 'F fms)       (== res 'F)]))))

(define acl-map
  (lambda (acl pkt matched-entry)
    (conde [(== acl '())
            (== matched-entry 'implicit-deny)]
           [(fresh (entry acl-rest eres)
              (== acl `(,entry . ,acl-rest))
              (entry-match? entry pkt eres)
              (conde [(== eres 'T)
                      (== matched-entry entry)]
                     [(== eres 'F)
                      (acl-map acl-rest pkt matched-entry)]))])))

(define acl-allow
  (lambda (acl pkt allowed-entry)
    (fresh (epkt)
      (== allowed-entry `(permit . ,epkt))
      (acl-map acl pkt allowed-entry))))
