#lang racket
(require "lang/kara.rkt")

(stream->list (stream-interleave (list (stream 1 2 3) (stream 4 5 6) (stream 7 8 9))))
