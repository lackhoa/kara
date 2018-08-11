#lang racket
(require "macro.rkt")
(provide new-queue front-queue enqueue! dequeue!)

; Data abstraction
(def (front-ptr queue) (car queue))
(def (set-front-ptr! queue item) (set-car! queue item))
(def (rear-ptr queue) (cdr queue))
(def (set-rear-ptr! queue item) (set-cdr! queue item))

(def (empty-queue? queue) (null? (front-ptr queue)))
(def (new-queue) (cons null null))

; Returns the item at the front of the queue
(def (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(def (enqueue! queue item)
  (let ([new-pair (cons item null)])
    (if (empty-queue? queue)
        (begin (set-front-ptr! queue new-pair)
               (set-rear-ptr! queue new-pair)
               queue)   
      (begin (set-cdr! (rear-ptr queue) new-pair)
             (set-rear-ptr! queue new-pair)
             queue)))) 

(def (dequeue! queue)
  (if (empty-queue? queue)
      (error "DELETE! called with an empty queue" queue)
    (begin
      (set-front-ptr! queue
                      (cdr (front-ptr queue)))
      queue)))
