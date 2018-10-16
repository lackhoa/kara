#lang racket
(require "lang/kara.rkt"
         "ctrl.rkt"
         "mol.rkt"
         "enum.rkt"
         "types.rkt")

(load!)
(repeat 10000
        (thunk
         (repeat 20 (thunk
                     (main!)
                     (displayln (num))
                     (displayln (can-nums))))
         (save)))
