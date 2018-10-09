#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "enum.rkt"
         "types.rkt"
         "main.rkt")

(for ([i  (in-range 1)])
  (pydisplay "Cycle number:" i)
  (time (com 1))
  (time (col 1))
  (displayln (num))
  (save))
