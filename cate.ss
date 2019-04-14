(define iso
  (lambda (f*)
    (inst
     '(forall f (exists g (and (1 (c g f))
                     (1 (c f g)))))
     f*)))
