;;; (semi-)Adjustable parameters
(define arcs
  ;; A Scheme list
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
  ;; A miniKanren term
  (lambda (x) (reverseo x x)))

(define main
  ;; A miniKanren program
  (lambda (solution)
    (search '[c] solution)))

;;; A* stuff
(define A*sort
  (l> sort  (lambda (x y) (< (node->cost x) (node->cost y)))))
(define A*merge
  (l> merge (lambda (x y) (< (node->cost x) (node->cost y)))))
(define queue-insert
  ;; Does A* sort, and sieves out all state conflicts, and
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
;; (define queue-insert
;;   ;; DFS
;;   (lambda (new-nodes^ qrest^)
;;     (append new-nodes^ qrest^)))

;; (define queue-insert
;;   ;; BFS
;;   (lambda (new-nodes^ qrest^)
;;     (append qrest^ new-nodes^)))


(define node->pcost  (f>> car car))
(define node->heu    (f>> car cdr))
(define node->cost   (lambda (n) (+ (node->pcost n)
                               (node->heu   n))))
(define node->name   (f>> cadr car))
(define node->state  (f>> cadr cadr))
(define node->prev   cddr)
(define make-node
  (lambda (path-cost heuristic name state prev)
    (cons (cons path-cost heuristic)
          (cons (list name state)
                prev))))
(define node-extend
  (lambda (weight name child node^)
    (make-node (+ (node->pcost node^)
                  weight)
               (heuristic child)
               name child
               (;; node^ without the costs
                cdr node^))))

;;; The framework
(define search
  (lambda (start-state solution)
    (search-core (;; Starting queue
                  list (make-node 0 (heuristic start-state)
                                  'start start-state '[]))
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
          (conde [(goal state^)  (project (node^)
                                   (== solution (cons (car node^)
                                                     (;; Reverse the path for viewing pleasure
                                                      reverse (cdr node^)))))]
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
                                          not (member state visited)))))))]
                         [queue  (queue-insert new-nodes
                                               (cdr queue^))])
                    (search-core queue
                                 (cons state^ visited)
                                 solution))])))))
