(define hier '([ip tcp udp icmp]
               [30 pc-c]))

(define mapo2
  (lambda (f l1 l2 out)
    (conde [(== l1 '()) (== out '())]
           [(fresh (a1 d1 a2 d2 fa d-out)
              (== l1 `(,a1 . ,d1))
              (== l2 `(,a2 . ,d2))
              (f a1 a2 fa)
              (== out `(,fa . ,d-out))
              (mapo2 f d1 d2 d-out))])))

(define packet?
  ;; A six-dimensional hypercube
  (lambda (p)
    (fresh (protocol src-adr src-port dst-adr dst-port flags)
      (== p `(,protocol ,src-adr ,src-port ,dst-adr ,dst-port ,flags)))))

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

(define lookup-hier
  (lambda (x hier-iter res)
    (conde [(== hier-iter '()) (== res #f)]
           [(fresh (a d y ys)
              (== hier-iter `(,a . ,d))
              (== a `(,y . ,ys))
              (conde [(== x y) (== res ys)]
                     [(=/= x y) (lookup-hier x d res)]))])))

(define any-covers?
  (lambda (ranges value res)
    (conde [(== ranges '()) (== res #f)]
           [(fresh (a d test)
              (== ranges `(,a . ,d))
              (covers? a value test)
              (conde [(== test #t) (== res #t)]
                     [(== test #f) (any-covers? d value res)]))])))

(define covers?
  (lambda (range value res)
    (conde
     [(== range '*) (== res #t)]
     [(=/= range '*)
      (conde [(== range value) (== res #t)]
             [(=/= range value)
              (fresh (lu)
                (lookup-hier range hier lu)
                (conde [(== lu #f) (== res #f)]
                       [(=/= lu #f) (any-covers? lu value res)]))])])))

(define entry-match?
  (lambda (entry pkt res)
    (fresh (eaction epkt fms)
      (== entry `(,eaction . ,epkt))
      (mapo2 covers? epkt pkt fms)
      (conde [(== fms '(#t #t #t #t #t #t)) (== res #t)]
             [(membero #f fms)             (== res #f)]))))

(define acl-map
  (lambda (acl pkt matched-entry)
    (conde [(== acl '())
            (== matched-entry '(deny * * * * * *))]
           [(fresh (entry acl-rest eres)
              (== acl `(,entry . ,acl-rest))
              (entry-match? entry pkt eres)
              (conde [(== eres #t)
                      (== matched-entry entry)]
                     [(== eres #f)
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

'([pc-a   r1-g]
  [r1-s   isp-s0]
  [isp-s1 r3-s]
  [r3-g   pc-c])

'([int-acl r1-g acl1 in]
  [int-acl r3-g acl2 in])

'(path pc-a [r1-s isp-s0 isp-s1 r3-s r3-g] pc-c)
;; User wants connection
;; There should be a connection if there is a path, and every acl on the path allows the traffic
