(define arcs
  (lambda (tail)
    (>> (run* (weight name head)
          (fresh (n)
            (lengtho tail n)
            (<o n (build-num 4)))
          (membero `(,weight ,name)
                   '[(1 i) (2 a) (3 b) (4 c)])
          (cro head name tail)))))

(define heuristic length)

(define goal
  (lambda (x) (reverseo x x)))

(define main
  (lambda (solution)
    (bfs '[c] solution)))

(define node->cost   car)
(define node->name   (f>> cadr car))
(define node->state  (f>> cadr cadr))
(define node->prev   cddr)
(define node-extend
  (lambda (weight name child node^)
    (let* ([cost^   (node->cost node^)]
           [state^  (node->state node^)]
           [heu     (- (heuristic child) (heuristic state^))]
           [cost    (+ cost^ weight heu)])
      (cons cost (cons (list name child)
                       (cdr node^))))))

(define bfs
  (lambda (start-state solution)
    (bfs-core (;; Starting queue
               list (;; Starting node with a computed cost,
                     ;; a dummy arc and the input start state
                     cons (heuristic start-state)
                          (list (list 'nil start-state))))
              '[]  ;; None visited
              solution)))

(define bfs-core
  (lambda (queue^ visited solution)
    ;; queue holds nodes
    ;; visited holds states
    ;; solution is a node
    (if (null? queue^)  fail
        (let* ([node^   (car queue^)]
               [state^  (node->state node^)])
          (conde [(goal state^)  (== solution node^)]
                 [;; Possibly ignore goal and keep looking
                  (let* ([new-nodes
                          (>> (arcs state^)
                              (l> map
                                  (l> apply
                                      (lambda (w n s) (node-extend w n s node^))))
                              (l> filter
                                  (lambda (node)
                                    (let ([state  (node->state node)])
                                      (and (;; Visited?
                                          not (member state visited))
                                         (;; Compete with the rest of the queue
                                          for-all (lambda (qnode)
                                              (not (and (equal? (node->state qnode)
                                                            state)
                                                    (<= (node->cost qnode)
                                                       (node->cost node)))))
                                            (cdr queue^)))))))]
                         [;; Purify the rest of the queue
                          qrest
                          (>> (cdr queue^)
                              (l> filter
                                  (lambda (qnode)
                                    (not (exists (lambda (new-node)
                                            (equal? (node->state new-node)
                                                    (node->state qnode)))
                                          new-nodes)))))]
                         [queue
                          (;; Insert new nodes based on cost
                           key-merge (key-sort new-nodes)
                                     qrest)])
                    (pairo queue)
                    (bfs-core queue
                              (cons state^ visited)
                              solution))])))))
