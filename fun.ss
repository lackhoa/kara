(run* (pr)
  `(,(carf pr) . ,(cdrf pr)))

(defun carf (lambda (pr) (fresh () (== `(,a . ,d) pr) a)))
(defun cdrf (lambda (pr) (fresh () (== `(,a . ,d) pr) d)))
(defun memberf
  (lambda (x ls)
    (condo
     [(== (carf ls) x) ls]
     [else (memberf x (cdrf ls))])))
(defun appendf
  (lambda (l1 l2)
    (condo
     [(== '() l1) ls]
     [else `(,(carf l1) . ,(appendf (cdrf l1) l2))])))
