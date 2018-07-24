(load "set.kar")

(stdisplay-n "In set => #t")
(in-set? 2 (seq->set (list 1 2 3 4)))

(stdisplay-n "In set with range => #f")
(in-set? 5 (seq->set (range 1 4)))

(stdisplay-n "Intersection => (Set (1 2 9))")
(seq->list (intersection-set (seq->set (range 0 10))
                             (seq->set (list 1 2 9 11))))

(stdisplay-n "Union => (Set (0 ... 12))")
(seq->list (union-set (seq->set (range 3 10))
                      (seq->set (list 0 1 2 9 11 12))))
