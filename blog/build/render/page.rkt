#lang racket/base

(require racket/contract
         racket/date
         racket/format
         racket/list
         racket/match
         threading
         (only-in xml xexpr?)

         "../../lang/metadata.rkt"
         "../../paths.rkt"
         "../metadata.rkt")

(provide (contract-out
          [page (-> #:title string? #:body xexpr? xexpr?)]
          [post-page (-> rendered-post? xexpr?)]
          [post-index-page (->i ([total-pages (and/c exact-integer? (>=/c 1))]
                                 [page-number (total-pages) (and/c exact-integer? (>=/c 1) (<=/c total-pages))]
                                 [posts (listof rendered-post?)])
                                [result xexpr?])]
          [tag-index-page (->i ([total-pages (and/c exact-integer? (>=/c 1))]
                                [page-number (total-pages) (and/c exact-integer? (>=/c 1) (<=/c total-pages))]
                                [tag string?]
                                [posts (listof rendered-post?)])
                               [result xexpr?])]))

(define (page #:title title #:body body)
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
       (body
         (header
           (nav ([role "navigation"] [class "navigation-bar"])
             (ul ([class "navigation-items left"])
               (li ([class "blog-title-header"])
                 (a ([href "/"]) "Alexis King")))
             (ul ([class "navigation-items center"]))
             (ul ([class "navigation-items right"])
               (li (a ([href "/"]) "Home"))
               (li (a ([href "/resume.html"]) "About Me")))))
         (section ([role "main"]) ,body)
         (footer
           (div ([class "copyright-notice"]) "© " ,(~a (date-year (current-date))) ", Alexis King")
           (div "Built with "
                (a ([href "https://docs.racket-lang.org/scribble/index.html"]) (strong "Scribble"))
                ", the Racket document preparation system.")
           (div "Feeds are available via " (a ([href "/feeds/all.atom.xml"]) "Atom")
                " or " (a ([href "/feeds/all.rss.xml"]) "RSS") "."))))))

(define (post-page info)
  (match-define (rendered-post title-str title date tags body) info)
  (page #:title title-str
        #:body `(div ([class "content"])
                  (article ([class "main"])
                    ,(build-post-header title date tags)
                    ,@body))))

(define (post-index-page total-pages page-number posts)
  (page #:title "Alexis King’s Blog"
        #:body `(div ([class "content"])
                  ,@(build-post-index index-path total-pages page-number posts))))

(define (tag-index-page total-pages page-number tag posts)
  (page #:title (~a "Posts tagged ‘" tag "’")
        #:body `(div ([class "content"])
                     (h1 ([class "tag-page-header"])
                         "Posts tagged " (em ,tag))
                     ,@(build-post-index (λ~>> (tag-index-path tag))
                                         total-pages page-number posts))))

(define (build-post-header title date tags)
  (define date-str (post-date->string date))
  `(header
     (h1 ([class "title"]) ,@title)
     (div ([class "date-and-tags"])
       (time ([datetime ,date-str]) ,date-str)
       " " (span ([style "margin: 0 5px"]) "⦿") " "
       ,@(~> (for/list ([tag (in-list tags)])
               `(a ([href ,(tag-index-path tag)]) ,tag))
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
