#lang racket/base

(require (for-syntax racket/base
                     racket/match
                     syntax/location)
         racket/contract
         racket/match
         racket/string
         (prefix-in scribble: scribble/base)
         scribble/base
         scribble/core
         scribble/decode
         scribble/html-properties
         (only-in scribble/manual deftech tech)
         syntax/parse/define
         threading

         "../paths.rkt"
         "footnote.rkt"
         "metadata.rkt")

(provide (all-from-out "footnote.rkt")
         (struct-out post-date)
         (struct-out post-tags)
         (struct-out table-rows)
         blog-post
         deftech
         tech
         (contract-out [section (->* [] [#:tag (or/c string? (listof string?) #f)
                                         #:tag-prefix (or/c string? module-path? #f)
                                         #:style (or/c style? symbol? (listof symbol?) #f)
                                         #:depth exact-nonnegative-integer?]
                                     #:rest (listof pre-content?)
                                     part-start?)]
                       [subsubsubsection (->* [] [#:tag (or/c string? (listof string?) #f)
                                                  #:tag-prefix (or/c string? module-path? #f)
                                                  #:style (or/c style? symbol? (listof symbol?) #f)]
                                              #:rest (listof pre-content?)
                                              part-start?)]
                       [seclink (->* [string?]
                                     [#:doc (or/c module-path? #f)
                                      #:post (or/c string? #f)
                                      #:tag-prefixes (or/c (listof string?) #f)
                                      #:indirect? any/c]
                                     #:rest (listof pre-content?)
                                     element?)]
                       [other-post (-> string? element?)]

                       [code (-> content? ... element?)]
                       [code-block (-> content? ... block?)]
                       (struct pygments-content ([source string?] [language string?]))
                       [pygments (-> #:language string? string? ... element?)]
                       [pygments-block (-> #:language string? string? ... block?)]
                       [haskell (-> string? ... element?)]
                       [haskell-block (-> string? ... block?)]

                       [blog-tag (-> string? element?)]

                       [wikipedia (-> pre-content? ... element?)]
                       [github-repo (-> string? element?)]
                       [github-repo* (-> string? pre-content? ... element?)]
                       [hackage-package (-> string? element?)]
                       [hackage-package* (-> string? pre-content? ... element?)]
                       [hackage-module (-> string? string? element?)]
                       [hackage-module* (-> string? string? pre-content? ... element?)]))

;; -----------------------------------------------------------------------------

(module* lang racket/base
  (require scribble/doclang2
           (except-in scribble/base seclink section)
           (submod ".."))
  (provide (all-from-out scribble/doclang2
                         scribble/base
                         (submod ".."))))

(module* reader syntax/module-reader
  #:language            get-lang-mod-path
  #:read                scribble:read-inside
  #:read-syntax         scribble:read-syntax-inside
  #:whole-body-readers? #t
  #:info                (scribble-base-reader-info)
  #:language-info       (scribble-base-language-info)

  (require (for-syntax racket/base)
           racket/runtime-path
           (prefix-in scribble: scribble/reader)
           (only-in scribble/base/reader
                    scribble-base-reader-info
                    scribble-base-language-info))

  (define-runtime-module-path-index lang-mpi '(submod ".." lang))
  (define (get-lang-mod-path)
    #;(cons 'submod (resolved-module-path-name
                     (module-path-index-resolve
                      lang-mpi
                      #f
                      #'(submod ".." lang))))
    ;; see <https://github.com/racket/racket/issues/4174>
    '(submod blog/lang/base lang)))

;; -----------------------------------------------------------------------------

(define (section #:tag [tag #f]
                 #:tag-prefix [prefix #f]
                 #:style [style #f]
                 #:depth [depth 0]
                 . pre-content)
  (~> (apply scribble:section pre-content
             #:tag tag #:tag-prefix prefix #:style style)
      (struct-copy part-start _ [depth depth])))

(define (subsubsubsection #:tag [tag #f]
                          #:tag-prefix [prefix #f]
                          #:style [style #f]
                          . pre-content)
  (apply section pre-content #:depth 3
         #:tag tag #:tag-prefix prefix #:style style))

(define (seclink tag
                 #:doc [module-path #f]
                 #:post [post-path #f]
                 #:tag-prefixes [prefixes #f]
                 #:indirect? [indirect? #f]
                 . pre-content)
  (apply scribble:seclink tag pre-content
         #:doc module-path
         #:tag-prefixes (if post-path
                            (cons (blog-post-path->tag-prefix post-path) (or prefixes '()))
                            prefixes)
         #:indirect? indirect?))

(define (other-post post-path)
  (seclink "top" #:post post-path))

;; -----------------------------------------------------------------------------

(define (code . content)
  (element (style #f (list (alt-tag "code"))) content))

(define (code-block . content)
  (paragraph (style #f (list (alt-tag "pre"))) (apply code content)))

(struct pygments-content (source language) #:transparent)

(define (pygments #:language language . strs)
  (element (style #f (list (pygments-content (string-append* strs) language))) '()))
(define (pygments-block #:language language . strs)
  (paragraph (style #f (list (pygments-content (string-append* strs) language))) '()))

(define (haskell . strs)
  (apply pygments #:language "haskell" strs))
(define (haskell-block . strs)
  (apply pygments-block #:language "haskell" strs))

;; -----------------------------------------------------------------------------

(define (blog-tag tag-str)
  (hyperlink (index-path #:tag tag-str) tag-str))

(define (add-blog-post-properties title date tags)
  (define title-style (title-decl-style title))
  (struct-copy title-decl title
               [style (style (style-name title-style)
                             (list* date
                                    (post-tags tags)
                                    (style-properties title-style)))]))

(define-for-syntax (infer-date stx)
  (match (path->string (syntax-source-file-name stx))
    [(regexp #px"^(\\d{4})-(\\d{2})-(\\d{2})-"
             (list _
                   (app string->number year)
                   (app string->number month)
                   (app string->number day)))
     #`(post-date '#,year '#,month '#,day)]
    [_ (raise-syntax-error #f "file name does not start with date" stx)]))

(define-syntax-parser blog-post
  [(_ {~alt {~once {~var title-expr (expr/c #'title-decl? #:name "title expression")}}
            {~once {~seq #:tags {~var post-tags (expr/c #'(listof string?) #:name "tags expression")}}}}
      ...)
   #`(add-blog-post-properties title-expr.c
                               #,(infer-date this-syntax)
                               post-tags.c)])

;; -----------------------------------------------------------------------------

(define (wikipedia . pre-content)
  (define content (decode-content pre-content))
  (define words
    (match (string-split (content->string content))
      ; capitalize the first word to match wikipedia naming conventions
      [(cons word words)
       (cons (string-append (string (char-upcase (string-ref word 0)))
                            (substring word 1))
             words)]
      ['() '()]))
  (hyperlink (string-append "https://en.wikipedia.org/wiki/" (string-join words "_")) content))

(define (github-repo user+repo)
  (match user+repo
    [(regexp #px"^[^/]+/(.+)$" (list _ repo))
     (github-repo* user+repo repo)]
    [repo
     (github-repo* repo repo)]))
(define (github-repo* repo . pre-content)
  (define user+repo (if (string-contains? repo "/") repo (string-append "lexi-lambda/" repo)))
  (apply hyperlink (string-append "https://github.com/" user+repo) pre-content))

;; -----------------------------------------------------------------------------
;; hackage

(define (hackage-package package-name)
  (hackage-package* package-name package-name))
(define (hackage-package* package-name . pre-content)
  (apply hyperlink (string-append "https://hackage.haskell.org/package/" package-name) pre-content))

(define (hackage-module package+version module-name)
  (hackage-module* package+version module-name (tt module-name)))
(define (hackage-module* package+version module-name . pre-content)
  (apply hyperlink
         (string-append "https://hackage.haskell.org/package/" package+version
                        "/docs/" (string-replace module-name "." "-") ".html")
         pre-content))
