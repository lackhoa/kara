#lang racket
(require "lang/kara.rkt"
         racket/struct)

(struct point (x y)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'fuckyou)
      (lambda (obj) (list (point-x obj)
                     (point-y obj)))))])

(def l (list 1))
(parameterize ([print-graph #t])
  (print (point #f #f)))
