(import (kara-lang main)
        (mol)
        (types))

;; (def (parse mol)
;;   (match (mol%-data mol)
;;     ['mp=>  (list (parse (list-ref (mol%-kids mol) 1))
;;                   (parse (list-ref (mol%-kids mol) 2)))]
;;     ['ai=>  'i]
;;     ['ak=>  'k]
;;     ['as=>  's]))

(define (type e)
  ;; e is a combinatory expression like ((i s) k)
  (cond [(list? e)  (let ([x  (car e)]
                          [y  (cadr e)])
                      (>> (up p '[0] (type x))
                          (lambda (m)
                            (up m '[1] (type y)))
                          (lambda (m)
                            (ref m '[2]))))]
        [else       (case e
                      [i     i]
                      [k     k]
                      [s     s]
                      [c     c]
                      [b     b]
                      [else  new-var  #|Just a variable|#])]))

(define cl
  (f> >> type clean))
