;; Packet: (protocol src-adr src-port dst-adr dst-port flags)
;; ACL entries: (deny/permit proto src-adr dst-adr dst-port flags)

(define acl-100
  '[(permit ip any any any any)])

(define item-matched
  (lambda (item value res)
    (conde [(conde [(== item 'any)]
                   [(== item y)])
            (== res 'T)]
           [(=/= 'any) (=/= item value)
            (== res 'F)])))

(define entry-matched
  (lambda (entry value res)
    (fresh (eaction eproto eadr edst-adr edst-port eflags)
      (== entry (list eaction eproto eadr edst-adr edst-port eflag))
      (conde [(item-matched proto eproto 'T)
              (item-matched src-adr esrc-adr 'T)
              (item-matched dst-adr edst-adr 'T)
              (item-matched dst-port edst-port 'T)
              (item-matched flags eflags 'T)]
             [(conde [(item-matched proto eproto 'F)]
                     [(item-matched src-adr esrc-adr 'F)]
                     [(item-matched dst-adr edst-adr 'F)]
                     [(item-matched dst-port edst-port 'F)]
                     [(item-matched flags eflags 'F)])]))))

(define acl-map
  (lambda (acl pkt action matched-entry)
    (fresh (proto src-adr dst-adr src-port dst-port flags)
      (== pkt (list proto src-adr src-port dst-adr dst-port flags))
      (let process ([acl acl])
        (conde [(nullo acl) (== action 'deny)
                (== matched-entry 'Implicit-deny)]
               [(fresh (entry acl-rest)
                  (== acl (cons entry acl-rest))
                  (fresh (emat)
                    (== entry-matched entry pkt emat)
                    (conde [(== emat 'T)
                            (== action eaction)
                            (== matched-entry entry)]
                           [(== emat 'F)
                            (acl-map acl-rest pkt action matched-entry)])))])))))

(define tcp-connection
  (lambda (src-adr dst-adr port)
    (fresh (dst-int src-int acl)
      (int-adr dst-int dst-adr)
      (int-adr src-int src-adr)
      (int-facing-acl src-int acl)
      (=/= src-int dst-int)
      (fresh (ignore)
        (acl-map acl `(6 ,src-adr ,dst-adr ,ignore ,port 0) 'permit)))))
