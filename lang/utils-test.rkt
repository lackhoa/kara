#lang racket

(require "macro.rkt"
         "utils.rkt"
         rackunit
         racket/generator)

(def g (generator () (yield 1) (yield 2) 3))
(gen-get g 3)
