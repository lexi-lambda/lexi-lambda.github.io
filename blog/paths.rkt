#lang racket/base

(require net/url
         racket/contract
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
          [site-path->url-string (->* [site-path?] [#:fragment (or/c string? #f)] string?)]
          [full-url (->* [site-path?] [#:fragment (or/c string? #f)] string?)]

          [index-path (->* [] [exact-positive-integer?
                               #:tag (or/c string? #f)
                               #:file? any/c]
                           site-path?)]
          [post-path (->* [post-date? string?] [#:file? any/c] site-path?)]
          [feed-path (->* [(or/c 'atom 'rss)] [#:tag (or/c string? #f)] site-path?)]))

(define-runtime-path build-dir-base "../build")
(define-runtime-path output-dir-base "../output")
(define-runtime-path posts-dir "posts")

(define build-dir (simplify-path build-dir-base #f))
(define output-dir (simplify-path output-dir-base #f))

(define (site-path? v)
  (and (string? v)
       (absolute-path? v)))

(define (site-path->url-string path #:fragment [fragment #f])
  (url->string (struct-copy url (path->url path)
                            [scheme #f]
                            [host #f]
                            [fragment fragment])))

(define (full-url path #:fragment [fragment #f])
  (url->string (struct-copy url (path->url path)
                            [scheme "https"]
                            [host "lexi-lambda.github.io"]
                            [fragment #f])))

(define (index-path [page-number 1]
                    #:tag [tag #f]
                    #:file? [file? #f])
  (if tag
      (~a "/tags/"
          (to-slug tag)
          (if (= page-number 1) "" (~a "-" page-number))
          ".html")
      (if (= page-number 1)
          (if file? "/index.html" "/")
          (~a "/index-" page-number ".html"))))

(define (post-path date title #:file? [file? #f])
  (~a "/blog/" (string-join (post-date->strings date) "/")
      "/" (to-slug title) "/" (if file? "index.html" "")))

(define (feed-path format #:tag [tag #f])
  (~a "/feeds/" (if tag (to-slug tag) "all") "." format ".xml"))
