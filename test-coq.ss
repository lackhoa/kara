(load "coq.ss")

(pp (prove plus0r))
(pp (freshen 'X `(,(make-ctx '() '(= X X*)))))
