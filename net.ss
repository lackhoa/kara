(define treeo
  (lambda (tree value children)
    (== `(,value . ,children) tree)))

(define hier
  ;; This is a forest
  '((ip (tcp (telnet) (www))
        (udp (dns))
        (icmp (echo) (echo-reply)))
    (adr
     (pub-net (isp))
     (priv-net (ten-net (vlan1 (pc-a))
                        (vlan2 (pc-c)))))
    (est (#t))))

(define tree-forest-findt-suit
  ;; Returns the subtree in `tree` whose root is the value v
  ;; It's beautiful that "res" is assigned outside of the letrec
  ;; How can we do THIS in Prolog?
  (lambda (v res)
    (;; Mutual tree recursion
     letrec ([tree-findt
              (lambda (tree)
                (lambda (found?)
                  (fresh (u children)
                    (treeo tree u children)
                    (condo
                     [;; Base case for tree found situation
                      (==t u v) (== #t found?) (== tree res)]
                     [else ((forest-findt children) found?)]))))]
             [forest-findt
              (lambda (forest)
                (lambda (found?)
                  (conde
                   [(== '() forest) (== #f found?)]
                   [(fresh (a d)
                      (== `(,a . ,d) forest)
                      (condo
                       [(tree-findt a) (== #t found?)]
                       [else ((forest-findt d) found?)]))])))])
      (values tree-findt forest-findt))))

(define tree-findt
  (lambda (tree v res)
    (let-values ([(tree-findt _forest-findt)
                  (tree-forest-findt-suit v res)])
      (tree-findt tree))))

(define forest-findt
  (lambda (forest v res)
    (let-values ([(_tree-findt forest-findt)
                  (tree-forest-findt-suit v res)])
      (forest-findt forest))))

(define supert
  ;; If v1 is not found in the hierarchy -> v2 must be same
  (lambda (v1 v2)
    (lambda (?)
      (fresh (v1-tree _t)
        ((disjt
          (==t v1 v2)
          (conjt (forest-findt hier v1 v1-tree)
                 (tree-findt v1-tree v2 _t)))
         ?)))))
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
     [(== acl '())
      (== '(deny * * * *) entry)]
     [(fresh (a d)
        (== `(,a . ,d) acl)
        (condo
         [(entry-matcht a pkt) (== a entry)]
         [else (acl-map d pkt entry)]))])))
