;; Works with the canonical mk version

;;; Macros & helpers
(define-syntax letv
  (syntax-rules ()
    [(_ (x* ...) b)
     (let ([x* (var 'x*)] ...)
       b)]))

(define copy
  (lambda (t)
    (let-values
        ([(res _met)
          (let copy ([t t] [met '()])
            (cond
             [(var? t)
              (cond
               [(assq t met)
                => (lambda (asso) (values (my-rhs asso) met))]
               [else
                (let ([v (var (vector-ref t 0))])
                  (values v `((,t ,v) . ,met)))])]

             [(pair? t)
              (let-values ([(a met+) (copy (car t) met)])
                (let-values ([(d met++) (copy (cdr t) met+)])
                  (values `(,a . ,d) met++)))]

             [else (values t met)]))])
      res)))
(define my-rhs cadr)

;;; Intelligent inference rules
(define subo
  ;; Uses dirty "project" to make out variable
  ;; "f" must be ground
  (lambda (x v f g)
    (fresh ()
      (let subo ([f f] [g g])
        (conde
         [(== x f)
          (project (f)
            (if (var? f) (== v g) fail))]
         [(=/= x f)
          (conde
           [(conde [(== `() f)] [(numbero f)] [(symbolo f)])
            (== f g)]
           [(fresh (fa fd ga gd)
              (== `(,fa . ,fd) f)
              (== `(,ga . ,gd) g)
              (subo fa ga)
              (subo fd gd))])])))))

(define indo
  (lambda (base step P)
    (fresh (X)
      (subo X 0 P base)
      (fresh (m Pm Psm)
        (== `(-> ,Pm ,Psm) step)
        (subo X m P Pm)
        (subo X `(S ,m) P Psm)))))

(define mpo (lambda (imp ante conse) (== `(-> ,ante ,conse) imp)))


;;; Axioms
(define refl (letv (X) `(= ,X ,X)))

(define eql
  (letv (L L* R)
    `(-> (= ,L ,R)
        (-> (= ,L ,L*) (= ,L* ,R)))))

(define eqr
  (letv (L R R*)
    `(-> (= ,L ,R)
        (-> (= ,R ,R*) (= ,L ,R*)))))

(define plus-eq
  (letv (X X* Y Y*)
    `(-> (= X X*)
        (-> (= ,Y ,Y*)
           (= (+ ,X ,Y) (+ ,X* ,Y*))))))

(define S-eq
  (letv (X X*)
    `(-> (= ,X ,X*)
        (= (S ,X) (S ,X*)))))

(define plus0 (letv (X) `(= (+ 0 ,X) ,X)))

(define plus-S (letv (X Y) `(= (+ (S ,X) ,Y)
                               (S (+ ,X ,Y)))))

(define plus0r (letv (X) `(= (+ ,X 0) ,X)))

(define =sym (letv (X Y) `(-> (= ,X ,Y) (= ,Y ,X))))
