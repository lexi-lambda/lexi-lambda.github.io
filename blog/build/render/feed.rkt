#lang racket/base

(require racket/contract
         racket/format
         racket/list
         racket/match
         (only-in xml xexpr/c xexpr->string)

         "../../lang/metadata.rkt"
         "../../paths.rkt"
         "../metadata.rkt"
         (only-in "page.rkt" index-page-title))

(provide (contract-out
          [feed (->* [(or/c 'atom 'rss) (listof rendered-post?)]
                     [#:tag (or/c string? #f)]
                     xexpr/c)]))

(define (feed type posts #:tag [tag #f])
  (match type
    ['atom
     `(feed ([xmlns "http://www.w3.org/2005/Atom"] [xml:lang "en"])
        (title ,(index-page-title #:tag tag))
        (link ([rel "self"] [href ,(full-url (feed-path 'atom #:tag tag))]))
        (link ([rel "alternate"] [href ,(full-url (index-path #:tag tag))]))
        (updated ,(post-date->rfc-3339-datetime (rendered-post-date (first posts))))
        ,@(for/list ([post (in-list posts)])
            (match-define (rendered-post title-str _ date tags body) post)
            `(entry
               (title ,title-str)
               (link ([rel "alternate"] [href ,(full-url (rendered-post-path post))]))
               (published ,(post-date->rfc-3339-datetime date))
               (updated ,(post-date->rfc-3339-datetime date))
               (author (name "Alexis King"))
               (content ([type "html"]) ,(xexpr->string `(article ,@body))))))]

    ['rss
     (define updated (post-date->rfc-822-date (rendered-post-date (first posts))))
     `(rss ([version "2.0"])
        (channel
         (title ,(index-page-title #:tag tag))
         (description ,(index-page-title #:tag tag))
         (link ,(full-url (index-path #:tag tag)))
         (pubDate ,updated)
         (lastBuildDate ,updated)
         (ttl "60")
         ,@(for/list ([post (in-list posts)])
             (match-define (rendered-post title-str _ date tags body) post)
             `(item
                (title ,title-str)
                (link ,(full-url (rendered-post-path post)))
                (guid ([isPermaLink "true"]) ,(full-url (rendered-post-path post)))
                (pubDate ,(post-date->rfc-822-date date))
                (description ,(xexpr->string `(article ,@body)))))))]))

(define (post-date->rfc-3339-datetime date)
  (~a (post-date->string date) "T00:00:00Z"))

;; The RSS spec demands that dates be in this somewhat odd RFC 822 format, and
;; this appears to be important for at least some actual RSS implementations;
;; see lexi-lambda.github.io#10.
(define (post-date->rfc-822-date date)
  (~a (~r (post-date-day date) #:min-width 2 #:pad-string "0")
      " " (vector-ref #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                      (sub1 (post-date-month date)))
      " " (post-date-year date)))
