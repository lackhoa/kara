#lang racket
(require "lang/kara.rkt")

(provide main)

(def (any-double? l)
  (for*/or ([i   (in-list l)]
            [i2  (in-list l)])
    (= i2 (* i 2))))

(def (main ch)
  (let* ([msg  (place-channel-get ch)]
         [id   (car msg)]
         [val  (cadr msg)])
    (place-channel-put ch
                       `(,id  ,(any-double? val)))))
