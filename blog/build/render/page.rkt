#lang racket/base

(require racket/contract
         racket/date
         racket/format
         racket/list
         racket/match
         threading
         (only-in xml xexpr/c)

         "../../lang/metadata.rkt"
         "../../paths.rkt"
         "../metadata.rkt"
         "util.rkt")

(provide (contract-out
          [page (->* [#:title string? #:body xexpr/c] [#:tag (or/c string? #f)] xexpr/c)]
          [standalone-page (-> #:title string? #:body (listof xexpr/c) xexpr/c)]
          [post-page (-> rendered-post? xexpr/c)]
          [index-page-title (->* [] [#:tag (or/c string? #f)] string?)]
          [index-page (->i ([total-pages exact-positive-integer?]
                            [page-number (total-pages) (and/c exact-positive-integer? (<=/c total-pages))]
                            [posts (listof rendered-post?)])
                           (#:tag [tag (or/c string? #f)])
                           [result xexpr/c])]))

(define (page #:title title #:body body #:tag [tag #f])
  `(html
     (head
       (meta ([charset "utf-8"]))
       (meta ([name "viewport"] [content "width=device-width, initial-scale=1"]))
       (title ,title)
       ,(stylesheet (~a "https://fonts.googleapis.com/css?family"
                        "=Merriweather+Sans:400,300,300italic,400italic,700,700italic,800,800italic"
                        "|Merriweather:400,300,300italic,400italic,700,700italic,900,900italic"
                        "|Fira+Code:300,400,500,600,700"))
       ,(stylesheet "/css/application.min.css")
       ,(stylesheet "/css/pygments.min.css")
       (link ([rel "alternate"] [type "application/atom+xml"] [title "Atom Feed"] [href ,(feed-path 'atom #:tag tag)]))
       (link ([rel "alternate"] [type "application/rss+xml"] [title "RSS Feed"] [href ,(feed-path 'rss #:tag tag)]))
       (body
         (header
           (nav ([role "navigation"] [class "navigation-bar"])
             (ul ([class "navigation-items left"])
               (li ([class "blog-title-header"])
                 (a ([href "/"]) "Alexis King")))
             (ul ([class "navigation-items center"]))
             (ul ([class "navigation-items right"])
               (li (a ([href "/"]) "Home"))
               (li (a ([href "/about.html"]) "About Me")))))
         (section ([role "main"]) ,body)
         (footer
           (div ([class "copyright-notice"]) "© " ,(~a (date-year (current-date))) ", Alexis King")
           (div "Built with "
                (a ([href "https://docs.racket-lang.org/scribble/index.html"]) (strong "Scribble"))
                ", the Racket document preparation system.")
           (div "Feeds are available via " (a ([href ,(feed-path 'atom)]) "Atom")
                " or " (a ([href ,(feed-path 'rss)]) "RSS") "."))))))

(define (standalone-page #:title title #:body body)
  (page #:title title
        #:body `(div ([class "content"])
                  (article ([class "main"]) ,@body))))

(define (post-page info)
  (match-define (rendered-post title-str title date tags body) info)
  (page #:title title-str
        #:body `(div ([class "content"])
                  (article ([class "main"])
                    ,(build-post-header title date tags)
                    ,@body))))

(define (index-page-title #:tag [tag #f])
  (~a (if tag (~a "Posts tagged ‘" tag "’ | ") "")
      "Alexis King’s Blog"))

(define (index-page total-pages page-number posts #:tag [tag #f])
  (page #:title (index-page-title #:tag tag)
        #:tag tag
        #:body `(div ([class "content"])
                  ,@(when/list tag
                      `(h1 ([class "tag-page-header"])
                           "Posts tagged " (em ,tag)))
                  ,@(build-post-index (λ~>> (index-path #:tag tag))
                                      total-pages page-number posts))))

(define (build-post-header title date tags)
  (define date-str (post-date->string date))
  `(header
     (h1 ([class "title"]) ,@title)
     (div ([class "date-and-tags"])
       (time ([datetime ,date-str]) ,date-str)
       " " (span ([style "margin: 0 5px"]) "⦿") " "
       ,@(~> (for/list ([tag (in-list tags)])
               `(a ([href ,(index-path #:tag tag)]) ,tag))
             (add-between ", ")))))

(define (build-post-index page-path total-pages page-number posts)
  `[,@(for/list ([post (in-list posts)])
        (build-post-index-entry post))
    (ul ([class "pagination"])
      ,(if (= page-number 1)
           '(li ([class "disabled"]) "←")
           `(li (a ([href ,(page-path (sub1 page-number))]) "←")))
      ,@(for/list ([i (in-range 1 (add1 total-pages))])
          `(li ,(if (= i page-number)
                    '([class "pagination-number active"])
                    '([class "pagination-number"]))
               (a ([href ,(page-path i)]) ,(~a i))))
      ,(if (= page-number total-pages)
           '(li ([class "disabled"]) "→")
           `(li (a ([href ,(page-path (add1 page-number))]) "→"))))])

(define (build-post-index-entry post)
  (match-define (rendered-post _ title date tags body) post)
  (define path (rendered-post-path post))
  `(article ([class "inline"])
     ,(build-post-header `[(a ([href ,path]) ,@title)] date tags)
     ; only render up to the start of the first section
     ,@(takef body (match-lambda [(cons 'h2 _) #f] [_ #t]))
     (p (a ([href ,path]) (span ([class "read-more-text"]) "Read more") " →"))))

(define (stylesheet href)
  `(link ([rel "stylesheet"] [type "text/css"] [href ,href])))
