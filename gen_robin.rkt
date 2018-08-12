#lang racket
(require "lang/kara.rkt"
         "engine.rkt"
         racket/generator)
(provide gen-interleave)

(def (gen-interleave gens)
  (generator ()
    (let loop ([engs (map proc->engine gens)])
      (if (null? engs)
          'DONE
        (let* ([focus  (car engs)]
               [saved  null]
               [gen    null]
               [others (cdr engs)])
          (if (pair? focus)
              ; There is saved progress in the first item
              (begin (set! saved (car focus))
                     (set! gen   (cdr focus)))
            ; If there's no saved progress then ignore it
            (begin (set! saved focus)
                   (set! gen   focus)))
            (saved 2
              (lam (value ticks)
                (if (eq? 'DONE value)
                    (loop others)
                  (begin
                    (yield value)
                    ; Completers get another go
                    (loop (cons gen others)))))
              ; Failure: save progress and shove it to the end
              (lam (resume-eng)
                (loop (append others
                              (list (cons resume-eng
                                          gen)))))))))))



;; Testingu!
(def (fib n)
  (check-timer)
  (if (< n 2)
      n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(def fib-gen1
  (generator ()
    (let loop ([n 4])
      (yield (fib n))
      (if (<= n 11)
          (loop (+ n 1))
        'DONE))))

(def fib-gen2
  (generator ()
    (let loop ([n 10])
      (yield (fib n))
      (if (>= n 4)
          (loop (- n 1))
        'DONE))))

(def g (gen-interleave (list fib-gen2 fib-gen1)))
(g) (g) (g) (g) (g) (g) (g) (g)
(g) (g) (g) (g) (g) (g) (g) (g)
(g) (g) (g) (g) (g) (g) (g) (g)
(g) (g) (g) (g) (g) (g) (g) (g)

;; (for/list ([i (in-producer (gen-interleave (list fib-gen1 fib-gen2)) 'DONE)]) i)
