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
    (def (same-down? mol1 mol2 path1 path2)
      (orb (member path1 (mol%-sync mol2)  #|explicitly synced|#)
           (let ([ctor  (mol%-data mol1)])
             (match (mol%-data mol2)
               [#f        #f]
               [(== ctor)  (let* ([kids1  (mol%-kids mol1)]
                                 [kids2  (mol%-kids mol2)]
                                 [klen   (length kids1)])
                            (andb (eq? klen
                                       (ctor-arity ctor)
                                       (length kids2))
                                  (for/andb ([kid1  kids1]
                                             [kid2  kids2]
                                             [i     (in-range klen)])
                                    (same-down? kid1 kid2
                                                `(,@path1 ,i)
                                                `(,@path2 ,i)))))]
               [_         #f]))))
    (trace same-down?)

    (let-values ([(post p1 p2)
                  (split-common-postfix path1 path2)])
      (let loop ([posti  post  #|Tracing the path down|#]
                 [p1i    p1]
                 [p2i    p2]
                 [mol1   (ref root p1)]
                 [mol2   (ref root p2)])
        (displayln "This is mol1")(displayln mol1)
        (andb (andb mol1 mol2  #|Both do exist|#)
              (match posti
                ['()              (same-down? mol1 mol2 p1i p2i)]
                [`(,pcar ,@pcdr)  (let ([kid1  (ref mol1 `[,pcar])]
                                        [kid2  (ref mol2 `[,pcar])])
                                    (orb (andb (not (orb kid1 kid2)  #|Both do not exist|#)
                                               (member p1i (mol%-sync mol2)  #|explicit sync|#))
                                         (loop pcdr
                                               kid1
                                               kid2
                                               `(,@p1i ,pcar)
                                               `(,@p2i ,pcar))))])))))

  (let ([cache  '()  #|The cache, in the sync list is long|#])
    (let loop ([path  '[]]
               [mol   model])
      (andb (match (mol%-data mol)
              [#f  #t]
              [md  (eq? md (ref-data ins path))]  #|Data|#)

            (let* ([sync-ls  (mol%-sync mol)]
                   [pct      (car sync-ls)])
              (orb (findf (lam (ci)  (member pct ci))
                          cache)
                   (andb (for/andb ([p  (cdr sync-ls)])
                           (same? ins pct p))
                         (cons! sync-ls cache))) #|Topology|#)

            (let ([kids  (mol%-kids mol)])
              (for/andb ([kid-path  (for/list ([i  (range (length kids))])
                                      (rcons path i))]
                         [kid       kids])
                (loop kid-path kid)  #|Recursion|#))))))

(def (complexity m)  ;; mol% -> nat
  (add1 (sum-list (map complexity
                       (mol%-kids m)))))

(def (collide reactor ort)
  ;; mol% -> [cmol%] -> [cmol%]
  ;; Returns ort, after colliding with reactor
  (def (log-discard ccs1 ccs2)
    ;; mol% -> mol% -> void
    (call-with-output-file "db/discard.rkt"
      #:exists 'append
      (lam (out)
        (dm ccs1 out) (displayln (make-string 80 #\<) out)
        (dm ccs2 out) (newline out))))

  (for/fold ([new-ort  '()]) ([orti  ort])
    (if (instance? (decompress orti)
                   reactor)
        new-ort
        (cons orti new-ort))))

(def (make-p fun arg)
  ;; mol% -> mol% -> cmol%
  (>> (pull p '[1] fun)
      (f> pull '[2] arg)
      (f> detach '[0]  #|get conclusion|#)
      compress))

(def (combine reactor ort)
  ;; mol% -> [cmol%] -> [cmol%]  (new formulas)
  (for/fold ([accu  '()]) ([orti  ort])
    (let ([orti  (decompress orti)])
      (append (exclude-false `(,(make-p orti reactor)
                               ,(make-p reactor orti)))
              accu))))
