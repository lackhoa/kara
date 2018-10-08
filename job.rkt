#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "enum.rkt"
         "types.rkt"
         "main.rkt")

(for ([i  (in-range 100)])
  (pydisplay "Cycle number:" i)
  (com)
  (col)
  (save))
