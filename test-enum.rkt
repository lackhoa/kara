#! /usr/bin/racket
#lang racket

(require "lang/kara.rkt"
         "mole.rkt"
         "enum.rkt"
         "types.rkt")

(def (dm mol [port (current-output-port)])
  (pdisplay (mol-repr mol) 35 port))

(call-with-output-file "data"
  #:exists 'truncate
  (lam (out)
    (for ([m (enum null axioms)])
      (dm m out)
      (displayln (ref-data m '[0 1]))
      (newline out))))
