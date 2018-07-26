; The engines return streams
(def stream-round-robin
     (lam (engs)
          (if (null? engs)
              null
            ((car engs) 1
             (lam ticks value)
             (if
                 (null? (lcdr value))
                 (cons (car value)
                       (delay (round-robin (cdr engs))))
               (cons (car value)
                     (delay (round-robin (cons (make-eng (cdr value))
                                               (cdr engs))))))))))
