(define treeo
  (lambda (tree value children)
    (== `(,value . ,children) tree)))

(define tree-findt
  (lambda (tree sub path)
    (letrec
        ([tree-findt
          (lambda (tree path^)
            (lambda (found?)
              (fresh (v u f g)
                (treeo tree v f)
                (treeo sub  u g)
                (condo
                 [(;; If root matches then children has to match
                   ==t u v)
                  (== f g)
                  (== #t found?) (reverseo path^ path)]
                 [else
                  ((forest-findt f `(,v . ,path^))
                   found?)]))))]

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

      (tree-findt tree '()))))

(define forest-findt
  (lambda (forest sub path)
    (tree-findt `(_ . ,forest) sub `(_ . ,path))))

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
      (== '(deny *p *a *a *e) entry)]
     [(fresh (a d)
        (== `(,a . ,d) acl)
        (condo
         [(entry-matcht a pkt) (== a entry)]
         [else (acl-mapo d pkt entry)]))])))

(define acl-map-geno
  (lambda (acl pkt entry)
    (fresh (ps)
      (acl-mapo acl pkt entry)
      (pkt-up*o pkt ps)
      (let loop ([ps ps])
        (conde
         [(== '() ps)]
         [(fresh (pa pd entry2)
            (== `(,pa . ,pd) ps)
            (acl-mapo acl pa entry2)
            ;; (reflect pa) (reflect entry2)
            (=/=-action entry entry2)
            (loop pd))])))))

(define =/=-action
  (lambda (e1 e2)
    (fresh (a1 _d1 a2 _d2)
      (== `(,a1 . ,_d1) e1)
      (== `(,a2 . ,_d2) e2)
      (=/= a1 a2))))

(define upt
  (lambda (v u)
    (lambda (has-up?)
      (fresh (path _f)
        (condo
         [(conjt
           (forest-findt hier `(,v . ,_f) path)
           (=/=t '() path))
          (== #t has-up?)
          (lasto path u)]
         [else (== #f has-up?)])))))

(define lasto
  (lambda (ls v)
    (fresh (d)
      (reverseo ls `(,v . ,d)))))

(define pkt-upo
  (lambda (p p+)
    (conde
     [(== '() p) (== '() p+)]
     [(fresh (pa pd p+a p+d)
        (== `(,pa . ,pd) p)
        (== `(,p+a . ,p+d) p+)
        (condo
         [(upt pa p+a) succeed]
         [else (== pa p+a)])
        (pkt-upo pd p+d))])))

(define pkt-up*o
  (lambda (p res)
    (fresh (p^)
      (pkt-upo p p^)
      (let loop ([pre '()] [p p] [p^ p^] [p^s '()])
        (conde
         [(== '() p) (== p^s res)]
         [(fresh (pa pd p^a p^d pre+)
            (== `(,pa  . ,pd)  p)
            (== `(,p^a . ,p^d) p^)
            (append*o `(,pre (,pa)) pre+)
            (condo
             [(==t p^a pa)
              (loop pre+ pd p^d p^s)]
             [else
              (fresh (new)
                (append*o `(,pre (,p^a) ,pd) new)
                (loop pre+ pd p^d `(,new . ,p^s)))]))])))))

(define appendo
  (lambda (l1 l2 l)
    (conde
     [(== '() l1) (== l l2)]
     [(fresh (l1a l1d ld)
        (== `(,l1a . ,l1d)l1)
        (== `(,l1a . ,ld) l)
        (appendo l1d l2 ld))])))

(define append*o
  (lambda (l* l)
    (conde
     [(== '() l*) (== '() l)]
     [(fresh (x)
        (== l* `(,x))
        (== l x))]
     [(fresh (l*a l*b l*d l*bd)
        (== `(,l*a ,l*b . ,l*d) l*)
        (appendo l*a l*bd l)
        (append*o `(,l*b . ,l*d) l*bd))])))
