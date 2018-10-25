(library (main-procs)
  (export cycle! load! save num can-nums view)
  (import (chezscheme) (kara-lang main)
          (mol) (enum) (types))

;;; Functions
  (define collide
    ;; mol% -> [mol%] -> [mol%]
    ;; Returns ort, after colliding with reactor
    (lambda (reactor ort)
      (filter (lambda (orti) (not (instance? orti reactor)))
              ort)))

  (define make-p
    ;; mol% -> mol% -> mol%
    (lambda (fun arg)
      (>> (up p '[0] fun)
          (f> up '[1] arg)
          (f> ref '[2]  #|take the conclusion|#))))

  (define combine
    ;; mol% -> [mol%] -> [mol%]  (new formulas)
    (lambda (reactor ort)
      (do ([accu  '()  (append (remq #f (let ([focus (car ort)])
                                          `(,(make-p focus reactor)
                                            ,(make-p reactor focus))))
                               accu)]
           [ort   ort  (cdr ort)])

          ((null? ort) accu))))

;;; State
  (define db '()      #| [mols] |#)

  (define the-reactor #f)  #|mol%|#

;;; State-altering procedures
  (define load!
    (let ([cmd   (string-append "sqlite 3 "
                                (db-file)
                                " "
                                (ort-table))]
          [out   (car (process cmd))])
      (read proc))
    (lambda () (set! db
            (read (open-input-file )))))

  (define num
    (lambda () (length db)))

  (define can-nums
    (lambda () (length candidates)))

  (define save
    (lambda ()
      (when (file-exists? (db-file))
        (delete-file (db-file)))

      (call-with-output-file (db-file)
        (lambda (out) (write db out)))))

  (define view
    (lambda ()
      (when (file-exists? (view-file))
        (delete-file (view-file)))
      (call-with-output-file (view-file)
        (lambda (out) (for-each (lambda (m) (display m out) (newline out) (newline out))
                           db)))))

  (define query
    (lambda (thm)
      (>> (exists (lambda (m)
               (instance? thm m))
             db)
          pydisplay)))

;;; The fun stuff
;;; Parameters (preferably strings)
  (define db-file    (make-parameter "db/data.db"))
  (define can-table  (make-parameter "can"))
  (define view-file  (make-parameter "log/view.ss"))
  (define can-lim    (make-parameter 10))

  (define get-can!
    ;; -> mol%
    (#|Delete the old candidate|#
     process
     (format "sqlite3 ~a \"DELETE FROM ~a WHERE data = (SELECT data FROM ~a LIMIT 1)\""
             (db-file)
             (can-table)
             (can-table)))

    (with-input-from-string
        (get-string-some (format "sqlite3 ~s \"SELECT data FROM ~a LIMIT 1\""
                                 (db-file)
                                 (can-table)))
      (eval `(quote ,(read)))))

  (define add-cans!
    (lambda (cans)
      (process
       (format "sqlite3 ~a \"INSERT INTO ~a VALUES ~a\""
               (db-file)
               (can-table)
               (map (lambda (can) (format "(~a)" can))
                    cans)))))

  (define cycle!
    (lambda ()
      (define select!
        #|Loop until we can find a new value for `the-reactor`|#
        (lambda ()
          (let try ()
            (>> (get-can!)
                (lambda (can)
                  (if (exists (lambda (m) (instance? can m))
                         db)
                      (try)
                      (set! the-reactor can)))))))

      (define col!
        (lambda ()
          (set! db (collide the-reactor db))))

      (define com!
        #|The only phase where db and candidates grow|#
        (lambda ()
          (add-cans! (combine the-reactor db))

          (>> (make-p the-reactor the-reactor)
              add-can!  #|Cover the blind spot|#)

          (set! db
            (cons (clean the-reactor) db)  #|db grows|#)))

      (if (select!)
          (begin (col!) (com!))
          #f))))


(top-level-program
 (import (chezscheme) (kara-lang main)
         (enum) (main-procs))

 (debug? #f)
;;; Jobs
 ;; (load!)
 ;; (view)
 (repeat 1
         (lambda ()
           (repeat 20 (lambda ()
                        (cycle!)
                        (pydisplay (num) (can-nums))))
           (save)))
 (view)
 )
