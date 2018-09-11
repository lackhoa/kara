#lang racket
(require "lang/kara.rkt")

(def (gen-get gen num
              [func (lam (x) (pdisplay x) (newline))])
  (match num
    [0              (void)]
    [(? positive?)  (match (gen)
                      ['DONE (void)]
                      [val   (func val)
                             (gen-get gen
                                      (sub1 num)
                                      func)])]
    [_              (error "Invalid number" num)]))

(gen-get (generator () (yield 1) (yield 2) (yield 3) 'DONE) 10)

(define (gen->list gen num)
  ;; Returns: a list
  (match num
    [0              null]
    [(? positive?)  (match (gen)
                      ['DONE  null]
                      [val    (cons val
                                    (gen->list gen (sub1 num)))])]
    [_              (error "Invalid number" num)]))

(gen->list (generator () (yield 1) (yield 2) (yield 3) 'DONE) 10)
