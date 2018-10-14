#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "types.rkt")

(provide (all-defined-out))

(def (height c/mol)
  ;; Used for synchronization
  (match c/mol
    [(list _ kids)    (match kids
                        ['()  0]
                        [_    (add1 (apply max (map height kids)))])]
    [(list _ _ kids)  (match kids
                        ['()  0]
                        [_    (add1 (apply max (map height kids)))])]))

(def (instance? ins model)
  ;; mol% -> mol% -> bool
  ;; Check if `ins` is an instance? of `model`
  (def (ctor-arity s)
    (case s
      [(->)   2]
      [else  0  #|This enables the use of arbitrary variables|#]))

  (def (same? root path1 path2)
    ;; mol% -> path -> path -> bool
    ;; the meaning of uttering "path1 is synced with path2"
    (let ([mol1  (ref root path1)]
          [mol2  (ref root path2)])
      (match (andb mol1 mol2)
        [#t  (orb (member path1 (mol%-sync mol2))
                  (let ([ctor  (mol%-data mol1)])
                    (match (mol%-data mol2)
                      [#f        #f]
                      [(== ctor)  (let ([kpaths1  (kids-paths root path1)]
                                       [kpaths2  (kids-paths root path2)])
                                   (andb (eq? (ctor-arity ctor)
                                              (length kpaths1)
                                              (length kpaths2))
                                         (for/andb ([kp1  kpaths1]
                                                    [kp2  kpaths2])
                                           (same? root kp1 kp2))))]
                      [_         #f])))]
        [#f  (andb (not (orb mol1 mol2)
                      #|If they were synced, both would exist|#)
                   (same? root
                          (rcdr path1)
                          (rcdr path2)))])))

  (let loop ([path  '[]])
    (andb (match (ref-data model path)
            [#f  #t]
            [md  (eq? md (ref-data ins path))]  #|Data|#)

          (let* ([sync-ls  (ref-sync model path)]
                 [pct      (car sync-ls)])
            (for/andb ([p  (cdr sync-ls)])
              (same? ins pct p)) #|Topology|#)

          (for/andb ([kid-path  (kids-paths model path)])
            (loop kid-path)  #|Recursion|#))))

(def (complexity m)  ;; mol% -> nat
  (add1 (sum-list (map complexity
                       (mol%-kids m)))))

(def (original? reactor ort)
  ;; mol% -> [cmol%] -> bool
  (for/andb ([orti  ort])
    (not (instance? reactor
                  (decompress orti)))))

(def ((place-version func) pch)
  (place-channel-put pch
                     (apply func
                       (place-channel-get pch))))

(def place-original?
  (place-version original?))

(def (collide reactor ort)
  ;; mol% -> [cmol%] -> [cmol%]  (when reactor is original)
  ;;                  | #f       (when it isn't)
  ;; Returns ort, after colliding with reactor
  (def (log-discard ccs1 ccs2)
    ;; mol% -> mol% -> void
    (call-with-output-file "db/discard.rkt"
      #:exists 'append
      (lam (out)
        (dm ccs1 out) (displayln (make-string 80 #\<) out)
        (dm ccs2 out) (newline out))))

  (for/fold ([new-ort  '()]) ([orti  ort])
    (match (instance? (decompress orti)
                      reactor)
      [#t  new-ort]
      [#f  (cons orti new-ort)])))

(def place-collide
  (place-version collide))

(def (make-mp fun arg)
  ;; mol% -> mol% -> cmol%
  (>> (pull mp '[1] fun)
      (lam (mp1)
        (pull mp1 '[2] arg))
      (lam (mp2)
        (compress (#|get conclusion|#
                   detach mp2 '[0])))))

(def (combine reactor ort)
  ;; mol% -> [cmol%] -> [cmol%]  (new formulas)
  (for/fold ([accu  '()]) ([orti  ort])
    (let ([orti  (decompress orti)])
      (append (exclude-false `(,(make-mp orti reactor)
                               ,(make-mp reactor orti)))
              accu))))

(def place-combine
  (place-version combine))
