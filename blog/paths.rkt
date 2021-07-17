#lang racket/base

(require racket/contract
         racket/format
         racket/runtime-path
         racket/string

         "build/render/util.rkt"
         "lang/metadata.rkt")

(provide (contract-out
          [build-dir path?]
          [output-dir path?]
          [posts-dir path?]

          [site-path? predicate/c]
          [index-path (->* [(and/c exact-integer? (>=/c 1))] [#:file? any/c] site-path?)]
          [post-path (->* [post-date? string?] [#:file? any/c] site-path?)]
          [tag-index-path (->* [string?] [(and/c exact-integer? (>=/c 1))] site-path?)]))

(define-runtime-path build-dir-base "../build")
(define-runtime-path output-dir-base "../output")
(define-runtime-path posts-dir "posts")

(define build-dir (simplify-path build-dir-base #f))
(define output-dir (simplify-path output-dir-base #f))

(define (site-path? v)
  (and (string? v)
       (absolute-path? v)))

(define (index-path page-number #:file? [file? #f])
  (if (= page-number 1)
      (if file? "/index.html" "/")
      (~a "/index-" page-number ".html")))

(define (post-path date title #:file? [file? #f])
  (~a "/blog/" (string-join (post-date->strings date) "/")
      "/" (to-slug title) "/" (if file? "index.html" "")))

(define (tag-index-path tag-str [page-number 1])
  (~a "/tags/"
      (string-replace tag-str " " "-")
      (if (= page-number 1) "" (~a "-" page-number))
      ".html"))
