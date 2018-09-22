#lang racket
(require "lang/kara.rkt"
         racket/hash)
(provide (all-defined-out))

;;; Molcules
(struct mol% ([data #:mutable  #|'no-dat | symbol|#]
              [kids #:mutable  #|list[mol | valid path]|#])
  #:prefab)

(def (new-mol)
  (mol% 'no-dat null))

(def (ref mol path)
  (match path
    ['[]             mol]
    [(cons nxt rst)  (let ([kids  (mol%-kids mol)])
                       (match (<= nxt (last-index kids))
                         [#t  (ref (list-ref kids nxt)
                                   rst)]
                         [#f  'not-found]))]))

(def (ref-data mol path)
  (match (ref mol path)
    ['not-found     'no-dat]
    [(mol% data _)  data]))

(def (ref-kids mol path)
  (match (ref mol path)
    ['not-found     null]
    [(mol% _ kids)  kids]))

(def (cascade mol proc)
  (let loop ([focus  mol])
    (proc focus)
    (for ([kid  (mol%-kids focus)])
      (loop kid))))

(def (update! mol path [val 'no-dat])
  ;; Can be used to both update and expand (when val = 'no-dat).
  (def (expand! mol next-id)
    (let ([kids (mol%-kids mol)])
      (set-mol%-kids! mol
                      (let ([fillers (build-list (- next-id
                                                    (last-index kids))
                                                 (const (new-mol)))])
                        (append kids fillers)))))

  (let loop ([m mol]  [p path])
    (match p
      ['[]             (unless (eq? val 'no-dat)
                         (match (mol%-data m)
                           [(== val)  (void)]
                           ['no-dat  (set-mol%-data! m val)]
                           [_        'conflict]))]
      [(cons nxt rst)  (match (ref mol (list nxt))
                         ['not-found (begin (expand! mol nxt)
                                            (update! (ref mol (list nxt))
                                                     rst
                                                     val))]
                         [kid        (update! kid rst val)])])))


(def (height mol)
  ;; Used for synchronization, useful for height limitation...
  (match (mol%-kids mol)
    [(list)  0]
    [kids    (add1 (apply max (map height kids)))]))

(def (sync! root p1 p2)
  (def (descendant? branch root)
    (let ([kids  (mol%-kids root)])
      (or (memq branch kids)
         (exists (lam (kid)  (descendant? branch kid))
            kids))))

  ;; Merge two molecules, if fail, returns 'conflict,
  ;; if successful, literally assign p2 to p1
  (begin
    #|Make sure the paths exist|#
    (update! root p1)
    (update! root p2))

  (let/ec escape
    (let ([mol1 (ref root p1)]
          [mol2 (ref root p2)])
      (when (or (descendant? mol1 mol2)
               (descendant? mol2 mol1))
        (escape 'conflict)  #|Cycle check|#)

      (when (eq? mol1 mol2)
        (escape (void))  #|Save some time|#)

      (let loop ([m1 mol1] [m2 mol2])


        (let ([d1  (mol%-data m1)]
              [d2  (mol%-data m2)])
          (match d1
            [(== d2)    (void)]
            ['no-dat   (update! m1 '[] d2)]
            [_         (escape 'conflict)]))


        (let ([i1  (last-index (mol%-kids m1))]
              [i2  (last-index (mol%-kids m2))])
          ;; Add the missing kids
          (when (< i1 i2)
            (update! m1 (list i2))))

        (for ([new-m1 (mol%-kids m1)]
              [new-m2 (mol%-kids m2)]
              #|m2 may be exhausted before m1|#)
          ;; Our job is over, let the kids sync
          (when (eq? (loop new-m1 new-m2)
                     'conflict)
            (escape 'conflict))))

      (cascade root
               (lam (mol)
                 (unless (memq mol `(,mol1 ,mol2))
                   (def new-kids
                     (let build ([accum  null]
                                 [ls     (mol%-kids mol)])
                       (match ls
                         ['()             (reverse accum)]
                         [(cons nxt rst)  (match (eq? nxt mol2)
                                            [#t  (build (cons mol1 accum) rst)]
                                            [#f  (build (cons nxt accum)  rst)])])))

                   (set-mol%-kids! mol new-kids)))  #|Identity exchange|#))))

;;; Functional stuff
(def (copy mol [path null])
  ;; Utilize the print graph ability
  ;; Can be used to detach
  (let-values ([(in out) (make-pipe)])
    (parameterize ([print-graph #t])
      (write (ref mol path) out))
    (read in)))

(def (update mol path [val 'no-dat])
  (let ([clone  (copy mol)])
    (match (update! clone path val)
      ['conflict  'conflict]
      [_          clone])))

(def (sync root p1 p2)
  (let ([clone  (copy root)])
    (match (sync! (copy root) p1 p2)
      ['conflict  'conflict]
      [_          clone])))

;;; Others
(def (pull root branch path)
  (let* ([unifier  (mol% 'no-dat `(,root ,branch))])
    (sync! unifier
           `[0 ,@path]
           '[1])
    (copy unifier '[0])))

(define-syntax-rule (pull! root branch path)
  (set! root (pull root branch path)))

;;; Printing
(def (dm mol [port  (current-output-port)])
  ;; Make it pretty
  (parameterize ([print-graph  #t])
    (pdisplay mol 35 port)))

(def (wm mol port)
  ;; Make it efficient
  (parameterize ([print-graph  #t])
    (write mol port)))
