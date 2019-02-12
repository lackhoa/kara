(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")
(load "faster-miniKanren/matche.scm")
(load "faster-miniKanren/numbers.scm")

(define pp (lambda (ls) (for-each pretty-print ls)))

(load "reif.ss")
;; This is my version
(load "full-interp.ss")

(time (run 500 (_.0 _.1 _.2 _.3)
        (evalo
         `(match ,_.0 [`,_.0 ,_.1] . ,_.2)
         _.1)))

;; This is the one in faster-mk
(load "faster-miniKanren/full-interp.scm")
(time (run 500 (_.0 _.1 _.2 _.3)
        (evalo
         `(match ,_.0 [`,_.0 ,_.1] . ,_.2)
         _.1)))
