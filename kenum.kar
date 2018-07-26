; Modes: #t: have to exhaust every stream
;        #f: only need to exhaust one stream
(def (juggle mode timed-streams deadline)
  (def first (car timed-streams))
  (def next  (cadr timed-streams))
  (def rest  (list-tail timed-streams 2))
  (cond
    [(< deadline (time-ms))
     (tag 'Timeout (lam (new-dl)
                     (juggle timed-streams new-dl)))]  
    [(null? first)
     (if mode
         (juggle (cdr timed-streams) deadline)
         ; If one is done, then the rest is also done
         null)]
    [else
     (case (car first)
       [(Val x) (cons x
                      (delay (juggle (cons (cdr first)
                                           rest)
                                     deadline)))]
       [(Timeout cont) (juggle (cons (next (+ (time-ms) 10))
                                     (append rest (list cont)))
                               deadline)])]))

(def (mole-enum mole deadline)
  ; Choose constructor and initialize the components.
  (def type (mole-get mole 'type))
  (def type-constructors (type-constructors (only type)))
  (def possible-constructors (atom-union (mole-get 'ctors) type-constructors))
  (def res-streams
       (map (lam (con)
              (lam (dl)
                (constructor-phase mole dl con)))
            possible-constructors))
  (juggle #t res-streams deadline))

(def (constructor-phase mole deadline constructor)
  ; Update constructor for the molecule
  (def mole (mole-set 'ctors (atom-wrap ctor)))
  (def model (ctor-model ctor))
  (def mole (update-kids mole (model-kids model)))
  (if (inconsistent mole)
      null
      ; The plan will be a list of starting nodes,
      ; with a list of relations.
      (def plans (analyze-relations (model-rels model)))
      (def res-streams
        (map (lam (plan)
               (lam (dl)
                 (relation-phase mole dl plan)
             plans))))
      (juggle #f res-streams deadline)))

(def (relation-phase mole deadline plan)
  (def begins    (car plan))
  (def relations (cadr plan))
  (relation-phase-cont deadline
                       (lam (dl)
                         (mole-enum-kids begins dl))
                       relations))

(def (relation-phase-cont deadline cont relations)
  (case (car (cont deadline))
    [(Val x)
     (cons (finishing-phase (apply-relations relations x))
           (delay (relation-phase-cont deadline
                                       (lam (dl) (cdr cont))
                                       relations)))]
    [(Timeout cont)
     (tag 'Timeout (lam (new-dl)
                     (relation-phase-cont new-dl
                                          cont)))]))

(def (mole-enum-kids mole kids deadline)
  (def pool (map mole-get kids))
  (mole-enum-kids-cont mole
                       kids
                       deadline
                       (mole-enum-many pool deadline)))

(def (mole-enum-kids-cont mole kids deadline cont)
  (def stream (cont deadline))
  (case (car stream)
    [(Val x)
     (cons (mole-set-many kids x)
           (delay (mole-enum-kids-cont mole
                                       kids
                                       deadline
                                       (cdr stream))))]
    [(Timeout cont)
     (tag 'Timeout (lam (new-dl)
                     (mole-enum-kids-cont mole
                                          kids
                                          new-dl
                                          cont)))]))

(def (mole-enum-many pool)
  )



