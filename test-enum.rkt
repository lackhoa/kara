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
    (for ([m  (cleanup (enum null axioms))])
      (dm m out)
      (newline out))))
