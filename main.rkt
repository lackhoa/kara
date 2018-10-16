#lang racket
(require "lang/kara.rkt"
         "ctrl.rkt"
         "mol.rkt"
         "enum.rkt"
         "types.rkt")


(begin (load!)
       (main!)
       (displayln (num))
       (displayln (can-nums))
       (save))
