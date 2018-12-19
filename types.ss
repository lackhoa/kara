(import (chezscheme)
        (kara-lang main))
(load "mol.ss")

;;; Utility functions
(define get-ccs
  (f> ref '[cdr car]))

(define get-prem
  (f> ref '[cdr cdr]))

(define parse
  (lambda (root)
    (let ([translate
           (let ([subs  '()]
                 [next  -1])
             (lambda (c)
               (cond [(null? c)               '()]
                     [(eq? c '_)              (begin (set! next (1+ next))
                                                     next)]
                     [(>> (symbol->string c)
                          (f> string-ref 0)
                          char-upper-case?)   (let ([lookup  (assq c subs)])
                                                (if lookup  (cdr lookup)
                                                    (begin (set! next (1+ next))
                                                           (set! subs (cons (cons c next)
                                                                            subs))
                                                           next)))]
                     [else                    c])))])

      (let loop ([exp  root])
        (cond [;; Negate numbers, since they clash with variables
               (integer? exp)  (string-append "n" (number->string exp))]
              [(atom? exp)     (translate exp)]
              [else            (cons (loop (car exp))
                                     (loop (cdr exp)))])))))

(define mk-proof
  (lambda (conclusion . premises)
    (>> `(=> ,conclusion
            ,@(let loop ([premises  premises])
                (if (null? premises)  (list)
                    (cons `(=> ,(car premises) . _)
                          (loop (cdr premises))))))
        parse)))

(define ls-proof
  (lambda ls
    (append (;; The fact that the axiom exists
             map (lambda (x)  `(=> (=> ,@x))) ls)
            (;; The version for deduction
             map (l> apply mk-proof) ls))))

;;; Combinators
(define i '(-> 0 0))

(define k '(-> 0 (-> 1 0)))

(define s
  '(-> (-> 0 (-> 1 2))
      (-> (-> 0 1)
         (-> 0 2))))

(define p '(=> (-> 0 1) 0 1))

(define mp
  '(=> (=> 11 22 (-> 0 1))
      (=> 33 44 0)
      1))

;;; Some more combinators
(define w
  '(-> (-> 0 (-> 0 1))
      (-> 0 1)))

(define c
  '(-> (-> 0 (-> 1 2))
      (-> 1 (-> 0 2))))

(define b
  '(-> (-> 1 2)
      (-> (-> 0 1)
         (-> 0 2))))

(define meta
  '(((derive . 0)
     (from () derive . 0))

    (;; Derivation of the truth
     (from _ derive))
    (;; Derivation from assumption
     (from 11 derive 1)
     (mem 1 11))
    (;; Derivation from axiom
     (from 10 derive 0)
     (=> 0 . 1) (from 10 derive . 1))
    (;; Derivation of many
     (from 2 derive 0 . 1)
     (from 2 derive 0) (from 2 derive . 1))))

(define equality
  '(((= 0 1)
     (= 1 0))

    (;; Equality path
     (= 1 2 . 22)
     (= 1 2) (=/= 1 2) (!mem 1 22) (= 2 . 22))))

(define category
  '((;; Composition intro 1
     (= (c 0 1) (c 2 1))
     (Map 1 4 5) (Map 0 5 6) (= 0 2))

    (;; Composition intro 2
     (= (c 0 1) (c 0 3))
     (Map 1 4 5) (Map 0 5 6) (= 1 3))

    (;; Associativity of composition
     (= (c (c 0 1) 2)
        (c 0 (c 1 2)))
     (Map 2 3 4) (Map 1 4 5) (Map 0 5 6))

    (;; Isomorphism 1
     (= (c 0 (- 0)) 2)
     (iMap 0) (Map 0 1 2))

    (;; Isomorphism 2
     (= (c (- 0) 0) 1)
     (iMap 0) (Map 0 1 2))

    (;; Identity map 1
     (= (c 0 1) 0)
     (Map 0 1 2))

    (;; Identity map 2
     (= (c 2 0) 0)
     (Map 0 1 2))))

(define misc
  '(((and))
    ((and X . Xs) X (and . Xs))
    ((or X . Xs) X)
    ((or X . Xs) (/+ X) (or . Xs)))
  )

(define apply-axioms
  '(((apply 3 1 3)
     (atom 3) (=/= 3 *))

    ((apply * 0 0))

    ((apply (1 . 11) 0 (2 . 22))
     (apply 1 0 2) (apply 11 0 22))))

(define list-axioms
  '(((mem 0 (0 . 1)))
    ((mem 0 (1 . 2))
     (mem 0 2))

    ((append () 0 0))
    ((append (1 . 10) 0 (1 . 2))
     (append 10 0 2))

    ((append-dl (++ 0 1)
                (++ 1 2)
                (++ 0 2)))

    ((last (0) 0))
    ((last (0 . 1) 2)
     (last 1 2))

    ((remove 1 () ()))
    ((remove 1 (1 . 10) 10))
    ((remove 1 (2 . 10) (2 . 9))
     (=/= 1 2) (remove 1 10 9))

    ((map 5 () ()))
    ((map 5 (1 . 10) (2 . 20))
     (apply 5 1 2) (map 5 10 20))

    ((forall 5 ()))
    ((forall 5 (1 . 10))
     (apply 5 1 2) 2 (forall 5 10))

    ((reverse 0 1)
     (reverse 0 () 1))
    ((reverse () 0 0))
    ((reverse (1 . 10) 0 2)
     (reverse 10 (1 . 0) 2))
    ))

(define knowledge
  '(((known 1)
     (remove 1 11 10) (forall (known *) 10) (related . 11))))

(define circuit
  '((;; Ohm's law
     (related . ((v 0 1) (i 0 1) (r 0 1))))

    (;; Kirchhoff's current law
     (related . 33)
     (all 0 . 88) (map (i *) 88 33))

    (;; Kirchhoff's voltage law
     (related . 33)
     (path (0 . 22) 0)
     (KVL 0 (0 . 22) 33))

    (;; KVL processing: last node
     (KVL 0 (1) ((v 1 0))))

    (;; KVL processing: intermediate nodes
     (KVL 0 (1 2 . 22) ((v 1 2) . 33))
     (KVL 0 (2 . 22) 33))

    (;; Assumption of resistance
     (known (r 0 1))
     (=/= 0 1) (res 2) (at 0 2) (at 1 2))

    ;; Paths (the last node is left out for a reason)
    (;; Path through one device
     (path (0) 1)
     (=/= 0 1) (at 0 8) (at 1 8))

    (;; Complex path
     (path (0 2 . 22) 1)
     (=/= 0 2) (=/= 2 1) (at 0 8) (at 2 8)
     ;; No loops
     (;; note that 0 can be 1
      !mem 0 22)
     (path (2 . 22) 1))
    ))

(define ca40
  '(((res r1) (res r2) (res r3) (res r4) (res r5))

    (all p1 r1 t1)
    (all p2 r1 r2 r4 r5)
    (all p3 r2 r3)
    (all p4 r3 r4 r5 t2)))

(define ca49
  '(((bat b p1 p2)
     (res r1) (res r2) (res r3) (res r4) (res r5))

    (all p1 r1 r3 b)
    (all p2 r2 r4 b)
    (all p3 r1 r2 r5)
    (all p4 r3 r4 r5)))

(define get-mount-points
  (lambda (point-lists)
    (flatmap (lambda (point-list)
               (let* ([point  (cadr point-list)]
                      [devs   (cddr point-list)])
                 (map (lambda (dev)  `(at ,point ,dev))
                      devs)))
             point-lists)))

(define parse-circuit
  (lambda (circ)
    (>> (append (car circ)
                (cdr circ)
                (get-mount-points (cdr circ)))
        (l> map list))))

(define anti-unify
  '(((anti-unify T1 T2 T)
     (;; Beginning of the computation
      anti-unify T1 T2 T () _))

    (;; The two terms are identical (literally, not just UNIFIABLE)
     (anti-unify T1 T2 T1
                 S S)
     (== T1 T2))

    (;; Pairs
     (;; This does NOT work properly on variables, as they'll be unified to lists
      anti-unify T1 T2 (T . Ts)
                 S S++)
     (=/= T1 T2)
     (!var T1) (!var T2)  ;; Variables can turn to lists, which is illegal in our case
     (unify T1 (T1h . T1tl))
     (unify T2 (T2h . T2tl))
     (anti-unify T1h T2h T
                 S S+)
     (anti-unify T1tl T2tl Ts
                 S+ S++))

    (;; Already substituted
     (anti-unify T1 T2 V
                 S S)
     (=/= T1 T2) (or (atom T1) (atom T2))
     (subs-lookup S T1 T2 V))

    (;; Reverse-substitute a fresh variable
     (anti-unify T1 T2 V
                 S ([T1 T2 -> V] . S))
     (=/= T1 T2) (or (atom T1) (atom T2))
     (/+ (subs-lookup S T1 T2 _)))

    ((subs-lookup ([T1 T2 -> V] . _)
                  Term1 Term2 V)
     (== T1 Term1) (== T2 Term2))

    ((subs-lookup ([_ _ -> _] . S)
                  Term1 Term2 V)
     (subs-lookup S Term1 Term2 V))
    ))

(define lgg
  '(((lgg (H1 :- . B1) (H2 :- . B2) (H :- . B))
     (;; Head
      anti-unify H1 H2 H [] S)
     (;; Bodies
      lgg-bodies B1 B2 [] B S S+))

    ;; Select literal from the first body
    ((lgg-bodies [] B2 B B S S))

    ((lgg-bodies [L . B1] B2 B B++ S S++)
     (lgg-b2 L B2 B B+ S S+)
     (lgg-bodies B1 B2 B+ B++ S+ S++))

    ;; and one from second body
    ((lgg-b2 B1 [] B B S S))

    ((lgg-b2 L1 [L2 . B2] B B+ S S++)
     (same-predicate L1 L2)
     (anti-unify L1 L2 L S S+)
     (lgg-b2 L1 B2 [L . B] B+ S+ S++))

    ((lgg-b2 L1 [L2 . B2] B B+ S S+)
     (/+ (same-predicate L1 L2))
     (lgg-b2 L1 B2 B B+ S S+))

    ((same-predicate L1 L2)
     (!var L1) (!var L2)
     (unify L1 [P1 . _]) (unify L2 [P2 . _])
     (== P1 P2))
    ))
