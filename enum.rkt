#lang racket
(require "lang/kara.rkt"
         "mol.rkt"
         "types.rkt"
         future-visualizer)

(provide (all-defined-out))

(def (instance? ins model)
  ;; Check if `ins` is an instance? of `model`
  (def (ctor-arity s)
    (case s
      [(->)   2]
      [else  0  #|This enables the use of arbitrary variables|#]))

  (def (same? root path1 path2)
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

  (orb (equal? ins model  #|Quick elimination|#)
       (let loop ([path  '[]])
         (andb (match (ref-data model path)
                 [#f  #t]
                 [md  (eq? md (ref-data ins path))]  #|Data|#)

               (let* ([sync-ls  (ref-sync model path)]
                      [pct      (car sync-ls)])
                 (for/andb ([p  (cdr sync-ls)])
                   (same? ins pct p)) #|Topology|#)

               (for/andb ([kid-path  (kids-paths model path)])
                 (loop kid-path)  #|Recursion|#)))))

(def (complexity m)
  (add1 (sum-list (map complexity
                       (mol%-kids m)))))

(def (conclusion cmol)
  ;; Just rips it out since it's the only node
  (decompress (first (cmol%-kids cmol))))

(def (collide reactor ort)
  ;; reactor: mol% (a conclusion)
  ;; ort    : [cmol%]
  ;; out    : [cmol%] (discarded, if reactor is original)
  ;;        | #f      (if reactor is unoriginal)
  (def (log-discard ccs1 ccs2)
    (call-with-output-file "db/discard.rkt"
      #:exists 'append
      (lam (out)
        (dm ccs1 out) (displayln "<<<<<<<" out)
        (dm ccs2 out) (newline out))))

  (>> (for/orb ([orti  ort])
        (instance? reactor
                   (conclusion orti))  #|Preliminary phase|#)

      (lam (_)
        (for/fold ([discarded  '()]) ([orti  ort])
          (match (instance? (conclusion orti)
                            reactor)
            [#f  discarded]
            [#t  (cons orti discarded)]))  #|Destruction phase|#)))

(def (combine reactor ort)
  ;; reactor: mol% (a conclusion)
  ;; ort    : [cmol%]
  ;; out    : [cmol%]
  (def (make-mp fun arg)
    (>> (pull mp '[1] fun)
        (lam (mol)
          (pull mol '[2] arg))
        (lam (mol)
          (let ([cm  (detach mol '[0]  #|Retain conclusion|#)])
            (cmol% 'mp=> `(,(compress cm))
                   #|compress|#)))))

  (for/fold ([accu  '()]) ([orti  ort])
    (let ([orti  (conclusion orti)])
      (append (exclude-false `(,(make-mp orti reactor)
                               ,(make-mp reactor orti)))
              accu))))
