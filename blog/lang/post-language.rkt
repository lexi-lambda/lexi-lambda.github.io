#lang at-exp racket/base

(require (for-syntax racket/base
                     racket/match
                     racket/syntax
                     syntax/location)
         racket/contract
         racket/match
         racket/string
         (prefix-in scribble: scribble/base)
         scribble/base
         scribble/core
         scribble/decode
         scribble/html-properties
         syntax/parse/define

         "../paths.rkt"
         "metadata.rkt")

(provide (struct-out post-date)
         infer-date
         (struct-out post-tags)
         define-footnote
         (contract-out [seclink (->* [string?]
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

                       (struct footnote-reference ([note-id symbol?]))
                       (struct footnote-definition ([note-id symbol?]))
                       [footnote-reference-element (-> symbol? element?)]
                       [footnote-flow (-> symbol? (listof block?) block?)]

                       [wikipedia (-> pre-content? ... element?)]
                       [github-repo (-> string? element?)]
                       [github-repo* (-> string? pre-content? ... element?)]
                       [hackage-package (-> string? element?)]
                       [hackage-package* (-> string? pre-content? ... element?)]
                       [hackage-module (-> string? string? element?)]
                       [hackage-module* (-> string? string? pre-content? ... element?)]))

;; -----------------------------------------------------------------------------

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

(define-syntax-parser infer-date
  [(_) (match (path->string (syntax-source-file-name this-syntax))
         [(regexp #px"^(\\d{4})-(\\d{2})-(\\d{2})-"
                  (list _
                        (app string->number year)
                        (app string->number month)
                        (app string->number day)))
          #`(post-date '#,year '#,month '#,day)]
         [_ (raise-syntax-error #f "file name does not start with date" this-syntax)])])

;; -----------------------------------------------------------------------------

(struct footnote-reference (note-id) #:transparent)
(struct footnote-definition (note-id) #:transparent)

(define (footnote-reference-element note-id)
  (element (style #f (list (footnote-reference note-id))) '()))

(define (footnote-flow note-id pre-flows)
  (nested-flow (style #f (list (footnote-definition note-id))) (decode-flow pre-flows)))

(define-simple-macro (define-footnote ref-id:id pre-flow ...)
  #:with note-ref-id (format-id #'ref-id "note:~a" #'ref-id #:subs? #t)
  (begin
    (define note-id (gensym 'ref-id))
    (define note-ref-id (footnote-reference-element note-id))
    (footnote-flow note-id (list pre-flow ...))))

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
