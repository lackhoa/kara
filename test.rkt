#lang racket
(require "lang/kara.rkt"
         racket/struct)

(struct point (x y))

(define write-proc
  (make-constructor-style-printer
   (lambda (obj) 'done)
   (lambda (obj) (list (point-x obj)
                  (point-y obj)))))

(parameterize ([print-graph #t])
  (println (point #f #f)))

(parameterize ([print-graph #t])
  (write-proc (point #f #f)
              (current-output-port)
              #t))
