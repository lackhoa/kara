#! /usr/bin/racket
#lang racket

(require "lang/kara.rkt"
         "mole.rkt"
         "enum.rkt")

(def dm (compose pdisplay mol-repr))

(call-with-output-file "data"
  #:exists 'truncate
  (lam (out)
    (write ai out)))

(call-with-input-file "data"
  (lam (in)
    (dm (read in))
    (dm (read in))
    (dm (read in))))
