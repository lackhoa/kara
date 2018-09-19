#lang racket
(require "lang/kara.rkt"
         "mole.rkt"
         "types.rkt")

(provide (all-defined-out))

(def (instance? r1 r2)
  ;; Returns true iff r1 is an instance of r2.
  (def (same? root p1 p2)
    (or (member p1 (ref-sync root p2))
       (and (eq? (ref-data root p1)
               (ref-data root p2))
          (eq? (length (ref-kids root p1))
               (length (ref-kids root p2))
               #|If this isn't true, the code below wouldn't work|#)
          (for/and ([pk1 (kids-paths root p1)]
                    [pk2 (kids-paths root p2)])
            (same? root pk1 pk2)))))

  (let loop ([path null])
    (and (or (eq? (ref-data r2 path)
               'no-dat)
          (eq? (ref-data r1 path)
               (ref-data r2 path)))

       (andmap (lam (p)  (same? r1 path p))
               (ref-sync r2 path))

       (<= (length (ref-kids r2 path))
          (length (ref-kids r1 path)))

       (andmap (lam (kid-path)  (loop kid-path))
               (kids-paths r2 path)))))

(def (make-mp fun arg)
  (pull (pull mp fun '[1]) arg '[2]))
