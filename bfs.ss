(define arcs
  (lambda (tail)
    (>> (run* (name head)
          (fresh (n)
            (lengtho tail n)
            (<o n (build-num 4)))
          (membero name '[i a b c])
          (cro head name tail))
        (l> map
            (l> apply
                (lambda (name head)
                  (list (case name
                          [i  1] [a  2] [b  3] [c  4])
                        name
                        head)))))))

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
    (let* ([cost^ (node->cost node^)])
      (cons (+ cost^ weight)
            (cons (list name child)
                  (cdr node^))))))

(define bfs
  (lambda (start-state solution)
    (bfs-core (;; Starting queue
               list (;; Starting node with cost 0, a dummy arc
                     ;; and the input start state
                     cons 0
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
                              (l> filter
                                  (l> apply
                                      (lambda (weight _ state)
                                        (and (;; Visited?
                                            not (member state visited))
                                           (;; Compete with the rest of the queue
                                            for-all (lambda (qnode)
                                                (not (and (equal? (node->state qnode)
                                                              state)
                                                      (<= (node->cost qnode)
                                                         (+ (node->cost node^) weight)))))
                                              (cdr queue^))))))
                              (l> map
                                  (l> apply
                                      (lambda (w n s) (node-extend w n s node^)))))]
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
