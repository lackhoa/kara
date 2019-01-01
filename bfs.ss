(define arc
  (lambda (tail weight name head)
    (fresh ()
      (== weight (build-num 1))
      (fresh (n)
        (lengtho tail n)
        (<o n (build-num 4)))
      (membero name '[a b c])
      (cro head name tail))))

(define goal
  (lambda (x) (reverseo x x)))

(define main
  (lambda (solution)
    (bfs '[i] solution)))

(define node->cost   car)
(define node->name   (f> cadr car))
(define node->state  (f> cadr cadr))
(define node->prev   cddr)
(define node-extend
  (lambda (weight name child node^
             node)
    (let* ([cost^ (node->cost node^)])
      (cons (+ cost^ weight)
            (cons (list name child)
                  (cdr node^))))))

(define bfs
  (lambda (start-state solution)
    (fresh ()
      (bfs-core (;; Starting queue
                 list (;; Starting node with cost 0, a dummy arc
                       ;; and the input start state
                       cons (build-num 0)
                            (list (list 'nil start-state))))
                '[]  ;; Visited
                solution))))

(define bfs-core
  (lambda (queue^ visited solution)
    ;; queue holds nodes
    ;; visited holds states
    ;; solution is a node
    (let ([node^   (car queue^)]
          [state^  (node->state node^)])
      (conda [(goal state^)
              (conde [;; Return
                      (== solution node^)]
                     [;; Keep looking
                      (bfs-core (cdr queue^)
                                (cons state^ visited)
                                solution)])]
             [;; NOT goal
              (let* ([new-nodes
                      (>> (run* (weight name state)
                            (arc state^ weight name state))
                          (l> filter
                              (lambda (weight _ state)
                                (and (not (member state visited))
                                   (for-all (lambda (qnode)
                                        (not (and (equal? (node->state qnode)
                                                      state)
                                              (<= (node->cost qnode)
                                                 (+ (node->cost node^) weight)))))
                                      (cdr queue)))))
                          (l> map (lambda (w n s)
                                    (node-extend w n s node^))))]
                     [qrest
                      (>> (cdr queue^)
                          (l> filter (lambda (qnode)
                                       (exists (lambda (new-node)
                                            (equal? (node->state new-node)
                                                    (node->state qnode)))
                                          new-nodes))))]
                     [queue
                      (;; Insert new nodes based on cost
                       key-merge new-nodes qrest)])
                (bfs-core queue
                          (cons state^ visited)
                          solution))]))))
