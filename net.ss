(define tree-forest-findt
  (lambda (sub)
    (letrec
        ([tree-findt
          (lambda (tree)
            (lambda (found?)
              (fresh (v u f g)
                (== tree `(,v . ,f))
                (== sub  `(,u . ,g))
                (condo
                 [(==t u v)
                  (== f g)
                  (== #t found?)]
                 [else
                  ((forest-findt f) found?)]))))]

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
  (lambda (tree sub)
    (let-values ([(t _f) (tree-forest-findt sub)])
      (t tree))))

(define forest-findt
  (lambda (forest sub)
    (let-values ([(_t f) (tree-forest-findt sub)])
      (f forest))))

(define supert
  (lambda (v1 v2)
    (lambda (?)
      (fresh (t1 _f1)
        (== t1 `(,v1 . ,_f1))
        ((forest-findt hier t1)     #t)
        ((tree-findt   t1   `(,v2)) ?)))))

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
;; (*p *a *a *e) is the most general
(define entry-matcht
  (lambda (entry pkt)
    (lambda (?)
      (fresh (eaction epkt)
        (== `(,eaction . ,epkt) entry)
        ((super*t epkt pkt) ?)))))

(define acl-mapo
  (lambda (acl pkt entry)
    (conde
     [(== '() acl)
      (== entry '[deny *p *a *a *e])
      (;; Ensures that the packet is well-formed
       (entry-matcht entry pkt)
       #t)]
     [(fresh (a d)
        (== `(,a . ,d) acl)
        (condo
         [(entry-matcht a pkt) (== a entry)]
         [else (acl-mapo d pkt entry)]))])))
