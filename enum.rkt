#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt")

(provide (all-defined-out))

(def ctors
  (let* ([ht (hash-set (hasheq) '-> 2)])
    ht))

(def (instance? ins model)
  ;; Returns true iff ins is an instance of model.
  (def (same? root p1 p2)
    (or (member p1 (ref-sync root p2))
       (let ([ctor  (ref-data root p1)])
         (match (ref-data root p2)
           ['no-dat  #f]
           [ctor     (and (eq? (length (ref-kids root p1))
                             (hash-ref ctors ctor))
                        (eq? (length (ref-kids root p2))
                             (hash-ref ctors ctor))
                        (for/and ([kp1  (kids-paths root p1)]
                                  [kp2  (kids-paths root p2)])
                          (same? root kp1 kp2)))]
           [_        #f]))))

  (let loop ([path null])
    (and (or (eq? (ref-data model path)
               'no-dat)
          (eq? (ref-data ins path)
               (ref-data model path)))

       (for/and ([p  (ref-sync model path)])
         (same? ins path p))

       (<= (length (ref-kids model path))
          (length (ref-kids ins   path)))

       (andmap (lam (kid-path)  (loop kid-path))
               (kids-paths model path)))))

(def (make-mp fun arg)
  (pull (pull mp fun '[1]) arg '[2]))
