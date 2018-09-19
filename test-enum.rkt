#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt"
         "enum.rkt")

(test-case
 "Generality"
 (check-eq? (instance? (new-root) (new-root))
            #t)
 (check-eq? (instance? (new-root) (update (new-root) '[] '->))
            #f)
 (check-eq? (instance? (update (new-root) '[] '->)
                       (update (new-root) '[] '->))
            #t)
 (check-eq? (instance? (update (update (new-root) '[0] '->)
                               '[1] '->)
                       (sync (new-root) '[0] '[1]))
            #t))
