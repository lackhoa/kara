#lang racket

(require "kara.rkt"
         "utils.rkt"
         rackunit)

"Will display 'Hello friend!'"
(stdisplay-n "Hello ~s!" 'friend)

(check-equal? '(S z) (tag 'S 'z) "Tagging")

