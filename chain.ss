(define arcs
  ;; A state is a number of links and lists representing chains
  (lambda (tail)
    (>> (run* (weight name head)
          (let ([open^    (car tail)]
                [chains^  (cadr tail)])
            (conde [;; Open one link
                    (== weight 2)
                    (fresh (c chains^^ c1 c2 new chains)
                      (== name `(break ,c into ,c1 and ,c2))
                      (select c chains^ chains^^)
                      (appendo* (list c1 c2 '[0]) c)
                      (rembero '[] [list c1 c2] new)
                      (appendo new chains^^ chains)
                      (== head (list (1+ open^) chains)))]
                   [;; Use one open link
                    (if (positive? open^) succeed fail)
                    (== weight 3)
                    (fresh (c c1 c2 chains^^ chains)
                      (== name `(join ,c1 and ,c2))
                      (select-many [list c1 c2] chains^ chains^^)
                      (appendo* [list c1 c2 '[0]] c)
                      (cro chains c chains^^)
                      (== head (list (1- open^) chains)))])))
        (;; Sort the chains, and remove duplicates
         l> map
            (l> apply
                (lambda (w n head)
                  (let ([open    (car head)]
                        [chains  (cadr head)])
                    (list w n (list open
                                    (sort
                                     (lambda (x y) (< (length x) (length y)))
                                     chains)))))))
        remove-duplicates)))


(define heuristic
  ;; Number of chains minus one
  (lambda (x) (1- (length (cadr x)))))

(define goal
  (lambda (x)
    (fresh (necklace)
      ;; We aim to make one necklace, with no open links left
      ;; However, we can't close a loop, so we need to make do
      ;; with one link and one long chain
      (== x `[1 [,necklace]]))))

(define main
  (lambda (solution)
    (search '[0 [[0 0 0] [0 0 0] [0 0 0] [0 0 0]]]
            solution)))
