#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "enum.rkt"
         "types.rkt"
         "main.rkt")

(repeat 1000000
        (com)
        (col)
        (save))
