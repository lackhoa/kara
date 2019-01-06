;;; Algorithms
;;; A* stuff
(define A*sort
  (l> sort  (lambda (x y) (< (node->cost x) (node->cost y)))))
(define A*merge
  (l> merge (lambda (x y) (< (node->cost x) (node->cost y)))))

(define A*insert
  ;; Does A* sort, and sieves out all state conflicts
  (lambda (new-nodes^ qrest^)
    (let* ([new-nodes  (filter (lambda (node)
                                 (let ([state  (node->state node)])
                                   (;; Compete with the rest of the queue
                                    for-all (lambda (qnode)
                                        (not (and (equal? (node->state qnode)
                                                      state)
                                              (<= (node->cost qnode)
                                                 (node->cost node)))))
                                      qrest^)))
                               new-nodes^)]
           [;; Purify qrest^
            qrest  (filter (lambda (qnode)
                             (not (exists (lambda (new-node)
                                     (equal? (node->state new-node)
                                             (node->state qnode)))
                                   new-nodes)))
                           qrest^)])
      (;; Insert new nodes based on cost
       A*merge (A*sort new-nodes) qrest))))

;;; Other strategies
(define dfs-insert
  ;; DFS
  (lambda (new-nodes^ qrest^)
    (append new-nodes^ qrest^)))

(define bfs-insert
  ;; BFS
  (lambda (new-nodes^ qrest^)
    (append qrest^ new-nodes^)))

;;; Greedy
(define greedy-sort
  (l> sort  (lambda (x y) (< (node->heu x) (node->heu y)))))
(define greedy-merge
  (l> merge (lambda (x y) (< (node->heu x) (node->heu y)))))

(define greedy-insert
  ;; Does greedy sort, and sieves out all state conflicts
  (lambda (new-nodes^ qrest)
    (let* ([new-nodes  (filter (lambda (node)
                                 (let ([state  (node->state node)])
                                   (not (exists (lambda (qnode)
                                           (equal? (node->state qnode)
                                                   state))
                                         qrest))))
                               new-nodes^)])
      (greedy-merge (greedy-sort new-nodes) qrest))))


;;; (semi-)Adjustable parameters
(define arcs
  ;; A Scheme list
  (make-parameter
   (lambda (tail)
     (run* (weight name head)
       (fresh (n)
         (<o n (build-num 4))
         (lengtho tail n))
       (membero `(,weight ,name)
                '[(1 i) (2 a) (3 b) (4 c)])
       (cro head name tail)))))

(define heuristic
  (make-parameter length))

(define goal
  ;; A miniKanren goal
  (make-parameter
   (lambda (x) (reverseo x x))))

(define start-state
  ;; A state
  (make-parameter
   '[c]))

(define queue-insert
  (make-parameter
   ;; A search strategy
   A*insert))


(define node->pcost  (f>> car car))
(define node->heu    (f>> car cdr))
(define node->cost   (lambda (n) (+ (node->pcost n)
                               (node->heu   n))))
(define node->name   (f>> cadr car))
(define node->state  (f>> cadr cadr))
(define node->prev   cddr)
(define make-node
  (lambda (path-cost heu name state prev)
    (cons (cons path-cost heu)
          (cons (list name state)
                prev))))
(define node-extend
  (lambda (weight name child node^)
    (make-node (+ (node->pcost node^)
                  weight)
               ((heuristic) child)
               name child
               (;; node^ without the costs
                cdr node^))))

;;; The framework
(define search
  (lambda (solution)
    (search-core (;; Starting queue
                  list (make-node 0 ((heuristic) (start-state))
                                  'start (start-state) '[]))
                 '[]  ;; None visited
                 solution)))

(define search-core
  (lambda (queue^ visited solution)
    ;; queue holds nodes
    ;; visited holds states
    ;; solution is a node
    (if (null? queue^)  fail
        (let* ([node^   (car queue^)]
               [state^  (node->state node^)])
          (conde [((goal) state^)  (== solution node^)]
                 [;; Possibly ignore goal and keep looking
                  (let* ([new-nodes
                          (>> ((arcs) state^)
                              (l> map
                                  (l> apply
                                      (lambda (w n s) (node-extend w n s node^))))
                              (l> filter
                                  (lambda (node)
                                    (let ([state  (node->state node)])
                                      (and (;; Visited?
                                          not (member state visited)))))))]
                         [queue  ((queue-insert) new-nodes (cdr queue^))])
                    (search-core queue
                                 (cons state^ visited)
                                 solution))])))))
