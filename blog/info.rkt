#lang info

(define collection "blog")

(define deps
  '("base"
    ["commonmark-lib" #:version "1.1"]
    "racket-index"
    "scribble-lib"
    "threading-lib"
    "web-server-lib"))
(define build-deps
  '("at-exp-lib"))
