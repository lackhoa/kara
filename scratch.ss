;; So what if I can get (3 a) to be on the left?

(((#(env q) ((#(y a) . #(b a)) . #(rest a))))
 ((((#(b a) (val . #(t q)))
    (#(y a) #(x q)))
   ()
   ())
  (((#(t q) (closure #(lam-expr r)
                     ((#(x q) . (rec . #(lam-expr r)))
                      .
                      #(rest a))))
    (#(b a) (rec . #(lam-expr r)))
    (#(y a) #(x q)))
   ()
   ())
  (()
   (((#(y a) #(x q))))
   ((lookupt #(x q) #(rest a) #(t q)) #t))))

(letrec ([e? (lambda (n) (if (= n 0) #t (o? (- n 1))))]
         [o? (lambda (n) (if (= n 0) #f (e? (- n 1))))])
  (let ([o? 5])
    (e? 1)))
