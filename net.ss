(define lookup
  (lambda (x ls res)
    (conde [(== ls '()) (== res #f)]
           [(fresh (a d y ys)
              (== ls `(,a . ,d))
              (== a `(,y . ,ys))
              (conde [(== x y) (== res ys)]
                     [(=/= x y) (lookup x d res)]))])))

(define mapo
  (lambda (f l out)
    (conde [(== l '()) (== out '())]
           [(fresh (a d fa d-out)
              (== l `(,a . ,d))
              (f a fa)
              (== out `(,fa . ,d-out))
              (mapo f d d-out))])))

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

(define travel
  (lambda (from-int path to-int)
    (conde [(== from-int to-int) (== path '[])]
           [(=/= from-int to-int)
            (;; commence the iterative deepening search
             travel-deepening `(,from-int out) path `(,to-int in) 0)])))

(define travel-deepening
  (lambda (from path to cost-lim)
    (conda [(travel-core from path to 0 cost-lim)]
           [(travel-deepening from path to (1+ cost-lim))])))

(define travel-core
  (lambda (from path to cost cost-lim)
    (if (> cost cost-lim) fail
        (fresh (from-int from-dir to-int _to-dir)
          (== from `(,from-int ,from-dir))
          (== to   `(,to-int ,_to-dir))
          (fresh (next-int next-dir rest-path)
            (;; The direct connection goes both ways
             conde [(lookup from-int direct-connections next-int)]
                   [(lookup next-int direct-connections from-int)])
            (conde [(== from-dir 'in)   (== next-dir 'out)]
                   [(== from-dir 'out) (== next-dir 'in)])
            (conde [(== next-int to-int) (== path '())]
                   [(=/= next-int to-int)
                    (== path `[(,next-int ,next-dir) . ,rest-path])
                    (travel-core `(,next-int ,next-dir) rest-path to
                                 (1+ cost) cost-lim)]))))))

(define tcp-connection
  (lambda (orig return min-allow)
    ;; orig and return are interfaces
    ;; min-allow is the most restrictive rules that
    ;; could be for each interface along the way
    (fresh (orig-adr return-adr packet1 packet2
                     path1^ path1 path2^ path2
                     min-allow1 min-allow2)
      (lookup orig   address-table orig-adr)
      (lookup return address-table return-adr)
      (== packet1 `(tcp ,orig-adr   * ,return-adr * 0))
      (== packet2 `(tcp ,return-adr * ,orig-adr   * 1))
      (travel orig path1^ return)
      (travel return path2^ orig)
      (appendo `[(,orig out)   . ,path1^] `[(,return in)] path1)
      (appendo `[(,return out) . ,path2^] `[(,orig in)]   path2)
      (mapo (lambda (int out) (== out `(,int : allow . ,packet1)))
            path1 min-allow1)
      (mapo (lambda (int out) (== out `(,int : allow . ,packet2)))
            path2 min-allow2)
      (appendo min-allow1 min-allow2 min-allow))))
