(load "atom.kar")

(stdisplay-n "Implicit Atom => #f, #t")
(def even-atom (tag 'Implicit even?))
(in-atom? 89 even-atom)
(in-atom? 74 even-atom)

(stdisplay-n "Explicit Atom => #t, #f")
(def atom-8 (tag 'Explicit (tag 'Set (range 0 8))))
(in-atom? 8 atom-8)
(in-atom? 59 atom-8)


