#lang racket
(require "lang/kara.rkt"
         racket/hash)
(provide mol-repr update sync new-root
         pull attach detach ref-data
         ref-sync ref-kids kids-paths
         dm height)

;;; Molcules
(struct mol% (sync [data #:mutable]
                   [kids #:mutable]) #:prefab)

(def (make-mol [data    'no-dat]
               [kids    null])
  ;; data     : 'no-dat | symbol
  ;; kids     : [mol | valid path]
  (mol% data kids))

(def (new-root)
  (make-mol))

(def (ref mol path)
  (match path
    ['[]             mol]
    [(cons nxt rst)  (let ([kids  (mol%-kids mol)])
                       (match (<= nxt (last-index kids))
                         [#t  (loop kid rst)]
                         [#f  'not-found]))]))

(def (ref-data mol path)
  (match (ref mol path)
    ['not-found     'no-dat]
    [(mol% data _)  data]))

(def (ref-kids mol path)
  (match (ref mol path)
    ['not-found     null]
    [(mol% _ kids)  kids]))

(def (update mol path [val 'no-dat])
  (def (expand mol next-id)
    (set-mol%-kids! mol
                    (let ([fillers (build-list (- nxt  (last-index (mol%-kids mol)))
                                               (lam (_)  (make-mol)))])
                      (append kids fillers))))

  (let loop ([p  path])
    (match path-left
      ['[]             'okay]
      [(cons nxt rst)
       (match (ref mol (list nxt))
         ['not-found (begin (expand mol nxt)
                            (update (ref mol (list nxt))
                                    rst))]
         [kid        (update kid rst)])])))

(def (height mol)
  ;; Used for synchronization
  (match (mol%-kids mol)
    [(list)  0]
    [kids    (add1 (apply max (map height kids)))]))

(def (sync root p1 p2)
  ;; Merge two molecules, if fail, returns 'conflict,
  ;; if successful, literally assign p2 to p1
  (begin
    #|Make sure the paths exist|#
    (update root p1)
    (update root p2))

  (def max-height
    #|watch the height to detect cycle|#
    (max (height (ref root p1))
         (height (ref root p2))))

  (let loop ([m1  (ref root p1)]  [m2  (ref root p2)])
    (let/ec escape
      (when (eq? m1 m2)
        #|Save ourselves some time here|#
        (escape root))

      (match* ((mol%-data m1)
               (mol%-data m2))
        [(x x)                (void)]
        [('no-dat other-dat)  (update m1 '[] other-dat)]
        [(dat 'no-dat)        (void)  #|No need to worry about this|#]
        [(_ _)                (escape 'conflict)])

      (let ([i1  (last-index (mol%-kids m1))]
            [i2  (last-index (mol%-kids m2))])
        ;; Add the missing kids
        (when (< i1 i2)
          (update m1 (list i2)))

        (when (> (height m1) max-height)
          (escape 'conflict)))

      (for ([new-m1 (mol%-kids m1)]
            [new-m2 (mol%-kids m2)]
            #|m2 may be exhausted before m1|#)
        ;; Our job is over, let the kids sync
        (when (eq? (loop new-fp1 new-fp2)
                   'conflict)
          (escape 'conflict)))))

  (let switch-id ([mol root])
    (unless (memq? mol (list m1 m2))
      (let replace ([accum  null]
                    [kids   (mol%kids mol)])
        (match kids
          ['()             (reverse accum)]
          [(cons nxt rst)  (match (eq? mol m2)
                             [#t  (replace (cons m1 accum)  rst)]
                             [#f  (replace (cons nxt accum) rst)])]))
      (for ([kid  (mol%kids mol)])
        (switch-id mol)))))

(def (migrate root from to)
  ;; Migrate the molecule from root-`from` to ?-`to`
  ;; Enables `attach` and `detach`
  (def (swap-prefix ls pre new-pre)
    (for/list ([li  (filter (lam (x) (list-prefix? pre x))
                            ls   #|Weed out the non-descendants|#)])
      (append new-pre
              (list-tail li (length pre)))))

  (let loop ([p  from])
    (mol% (swap-prefix (ref-sync root p)  from  to)
          (ref-data root p)
          (for/list ([kid-path  (kids-paths root p)])
            (loop kid-path)))))

(def (attach root branch to)
  (replace (update root to)
           to
           (migrate branch '[] to)))

(def (detach root from)
  ;; Convert root-path into a root
  (migrate root from '[]))

(def (pull host  guest  [to null])
  ;; It's like synchronizing, but with different roots
  (let* ([unifier (new-root)]
         [unifier (attach unifier host  '[0])]
         [unifier (attach unifier guest '[1])])
    (match (sync unifier
                 (append '[0] to)
                 '[1])
      ['conflict  'conflict]
      [unified    (detach unified '[0])])))

(def (dm mol [port (current-output-port)])
  (pdisplay (mol-repr mol) 35 port))
