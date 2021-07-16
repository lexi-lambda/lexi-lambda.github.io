#lang racket/base

(require racket/date
         racket/format)

(provide page)

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
               (li ([id "blog-title-header"])
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

(define (stylesheet href)
  `(link ([rel "stylesheet"] [type "text/css"] [href ,href])))
