#! /usr/bin/racket
#lang racket

(require "lang/kara.rkt"
         "mole.rkt"
         "enum.rkt"
         "types.rkt")

(def (dm mol [port (current-output-port)])
  (pdisplay (mol-repr mol) 35 port))

(let ([result (main)])
  (call-with-output-file "data"
    ;; Initialize
    #:exists 'truncate
    (lam (out)
      (write result out)))
  (call-with-output-file "debug"
    #:exists 'truncate
    (lam (out)
      (for ([m (append (car result)
                       (cdr result))])
        (dm m out)
        (newline out)))))

;; (call-with-input-file "data"
;;   (lam (in)
;;     (let ([old-new (read in)])
;;       (let ([result (main (car old-new)
;;                           (cdr old-new))])
;;         ;; Write the data
;;         (call-with-output-file "data1"
;;           #:exists 'truncate
;;           (lam (out)
;;             (write result out)))

;;         (call-with-output-file "debug1"
;;           #:exists 'truncate
;;           (lam (out)
;;             (for ([m (cdr result)])
;;               ;; Display for human
;;               (dm m out)
;;               (newline out))))))))
