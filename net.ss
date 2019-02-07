;; Packet = (protocol, source-ip, destination-ip, flags)

(define lookup
  (lambda (x ls res)
    (conde [(== ls '()) (== res #f)]
           [(fresh (a d y ys)
              (== ls `(,a . ,d))
              (== a `(,y . ,ys))
              (conde [(== x y) (== res ys)]
                     [(=/= x y) (lookup x d res)]))])))

(define packet?
  ;; A four-tuple
  (lambda (p)
    (fresh (protocol src-adr dst-adr flags)
      (== p `(,protocol ,src-adr ,dst-adr ,flags)))))

(define entry?
  ;; An entry is just a packet with an action attached
  (lambda (e)
    (fresh (action proto src-adr dst-adr flags)
      (== e `(,action ,proto ,src-adr ,dst-adr ,flags)))))

(define acl?
  (lambda (acl)
    (conde [(== acl '())]
           [(fresh (a d)
              (== acl `(,a . ,d))
              (entry? a) (acl? d))])))

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
                (lookup range hier lu)
                (conde [(== lu #f) (== res #f)]
                       [(=/= lu #f) (any-covers? lu value res)]))])])))

(define entry-match?
  (lambda (entry pkt res)
    (fresh (eaction epkt fms)
      (== entry `(,eaction . ,epkt))
      (mapo2 covers? epkt pkt fms)
      (conde [(== fms '(#t #t #t #t)) (== res #t)]
             [(membero #f fms)       (== res #f)]))))

(define acl-map
  (lambda (acl pkt matched-entry)
    (conde [(== acl '())
            (== matched-entry '(deny * * * *))]
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

(define acl100
  '([permit www ok-adr   * established]
    [deny   tcp *        * *]
    [permit *   good-adr * *]
    ))

(define hier '([tcp telnet www]
               [udp dns]
               [icmp echo echo-reply]))
