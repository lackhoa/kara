#lang racket
(require "../lang/kara.rkt"
         "../mol.rkt"
         "../enum.rkt"
         "../types.rkt"
         "../main.rkt")

(def FILE-NAME
  "data2.rkt")

(when (file-exists? FILE-NAME)
  (load FILE-NAME))

(for ([i  (in-range 100)])
  (pydisplay "Cycle number:" i)
  (time (com 100))
  (time (col 1))
  (displayln (num))
  (save FILE-NAME))
