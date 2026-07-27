#lang info

(define collection "blog")

(define deps
  '("at-exp-lib"
    "base"
    ["commonmark-lib" #:version "1.1"]
    "racket-index"
    "scribble-lib"
    "threading-lib"
    "web-server-lib"))

(define build-deps
  '())
