#lang racket/base

(require commonmark/parse
         (prefix-in md: commonmark/struct)
         racket/contract
         racket/format
         racket/list
         racket/match
         racket/string
         scribble/base
         scribble/core
         scribble/decode
         scribble/decode-struct
         scribble/html-properties
         threading
         (only-in xml
                  cdata
                  string->xexpr)

         (only-in "lang/base.rkt"
                  code
                  code-block
                  footnote-collect-element
                  footnote-ref
                  footnotes-section
                  post-date
                  post-tags
                  pygments-block))

(provide (contract-out
          [parse-markdown-post (-> input-port? part?)]))

;; -----------------------------------------------------------------------------

(define (parse-markdown-post in)
  (parameterize ([current-parse-footnotes? #t])
    (define title-info (parse-title-info in))
    (define doc (read-document in))
    (document->part doc title-info)))

(define (parse-title-info in)
  (match-define (list _ title-bytes) (regexp-match #px"^    Title: ([^\n]+)\n" in))
  (match-define (list _ year-bytes month-bytes day-bytes)
    (regexp-match #px"^    Date: ([0-9]{4})-([0-9]{2})-([0-9]{2})[^\n]*\n" in))
  (match-define (list _ tags-bytes)
    (regexp-match #px"^    Tags: ([^,\n]+(?:, [^,\n]+)*)\n\n" in))

  (match-define (md:document (list (md:paragraph title-content)) '())
    (string->document (bytes->string/utf-8 title-bytes)))
  (define tags (string-split (bytes->string/utf-8 tags-bytes) ", " #:trim? #f))

  (title #:style (style #f (list (post-date (bytes->number year-bytes)
                                            (bytes->number month-bytes)
                                            (bytes->number day-bytes))
                                 (post-tags tags)))
         (render-inline title-content)))

(define (bytes->number bs)
  (string->number (bytes->string/utf-8 bs)))

;; -----------------------------------------------------------------------------

(define (document->part doc title-decl)
  (define part-info (part-start 0
                                (title-decl-tag-prefix title-decl)
                                (title-decl-tags title-decl)
                                (title-decl-style title-decl)
                                (title-decl-content title-decl)))
  (match-define-values [main-part '()] (render-part (md:document-blocks doc) part-info))
  (struct-copy part main-part
               [to-collect (append (render-footnote-definitions (md:document-footnotes doc))
                                   (part-to-collect main-part))]
               [parts (append (part-parts main-part) (list (footnotes-section)))]))

(define (render-part blocks part-info)
  (define (collect-initial-blocks initial-blocks blocks)
    (match blocks
      ['() (finish-part (reverse initial-blocks) '() '())]
      [(cons (md:heading _ depth) _)
       (if (> depth (part-start-depth part-info))
           (collect-sub-parts (reverse initial-blocks) '() blocks)
           (finish-part (reverse initial-blocks) '() blocks))]
      [(cons block blocks)
       (collect-initial-blocks (cons (render-block block) initial-blocks) blocks)]))

  (define (collect-sub-parts initial-blocks parts blocks)
    (match blocks
      [(cons (md:heading content depth) blocks)
       #:when (> depth (part-start-depth part-info))
       (define-values [part blocks*]
         (render-part blocks
                      (make-part-info #:title (render-inline content)
                                      #:depth (add1 (part-start-depth part-info)))))
       (collect-sub-parts initial-blocks (cons part parts) blocks*)]
      [_
       (finish-part initial-blocks (reverse parts) blocks)]))

  (define (finish-part initial-blocks parts blocks)
    (define part-tags (if (empty? (part-start-tags part-info))
                          (list `(part ,(make-generated-tag)))
                          (part-start-tags part-info)))
    (define title-content (part-start-title part-info))
    (values (part (part-start-tag-prefix part-info)
                  part-tags
                  title-content
                  (part-start-style part-info)
                  (make-part-to-collect (first part-tags) title-content)
                  initial-blocks
                  parts)
            blocks))

  (collect-initial-blocks '() blocks))

(define (render-block block)
  (match block
    [(? md:thematic-break?)
     (paragraph (style #f (list (alt-tag "hr"))) '())]
    [(md:code-block content info-string)
     (define language (and~>> info-string (regexp-match #px"^[^ \t\r\n]+") first))
     (if language
         (pygments-block content #:language language)
         (code-block content))]
    [(md:html-block content)
     (xexpr->block (string->xexpr content))]
    [(md:paragraph content)
     (paragraph plain (render-inline content))]
    [(md:blockquote blocks)
     (nested-flow (style 'nested '()) (render-flow blocks))]
    [(md:itemization blockss _ start-num)
     (itemization (match start-num
                    [#f plain]
                    [1 (style 'ordered '())]
                    [_ (style 'ordered (list (attributes (list (cons 'start (~a start-num))))))])
                  (map render-flow blockss))]))

(define (render-flow blocks)
  (map render-block blocks))

(define (render-inline content)
  (match content
    [(? string?) content]
    [(? list?) (render-inlines content)]
    [(? md:line-break?) (linebreak)]
    [(md:bold content) (element 'bold (render-inline content))]
    [(md:italic content) (element 'italic (render-inline content))]
    [(md:code content) (code content)]
    [(md:link content dest _)
     (element (style #f (list (make-target-url dest))) (render-inline content))]
    [(md:image description source title)
     (image-element #f (render-inline description) source '() 1)]
    [(md:html content)
     (raise-arguments-error 'render-inline "unhandled HTML span" "content" content)]
    [(md:footnote-reference label)
     (footnote-ref label)]))

; Hacky special case for some inline HTML that shows up in a couple posts.
(define (render-inlines contents)
  (match contents
    ['() '()]
    [(cons (md:html "<sup>") contents)
     (define-values [inner-contents contents*] (scan-inlines/html-close contents "</sup>"))
     (cons (superscript (render-inlines inner-contents)) (render-inlines contents*))]
    [(cons (md:html "<code>") contents)
     (define-values [inner-contents contents*] (scan-inlines/html-close contents "</code>"))
     (cons (code (render-inlines inner-contents)) (render-inlines contents*))]
    [(cons content contents)
     (cons (render-inline content) (render-inlines contents))]))
(define (scan-inlines/html-close contents close-tag)
  (let loop ([inner-contents '()]
             [contents contents])
    (match contents
      ['() (values (reverse inner-contents) '())]
      [(cons (md:html str) contents)
       #:when (string=? str close-tag)
       (values (reverse inner-contents) contents)]
      [(cons inner-content contents)
       (loop (cons inner-content inner-contents) contents)])))

(define (xexpr->block xexpr)
  (match xexpr
    [(list (? symbol? tag) (list (list (? symbol? attr-name) (? string? attr-val)) ...) xexprs ...)
     (paragraph (style #f (list (alt-tag (symbol->string tag))
                                (attributes (map cons attr-name attr-val))))
                (map xexpr->content xexprs))]))
(define (xexpr->content xexpr)
  (match xexpr
    [(? string?) xexpr]
    [(list (? symbol? tag) (list (list (? symbol? attr-name) (? string? attr-val)) ...) xexpr ...)
     (element (style #f (list (alt-tag (symbol->string tag))
                              (attributes (map cons attr-name attr-val))))
              (map xexpr->content xexpr))]))

(define (render-footnote-definitions defns)
  (for/list ([defn (in-list defns)])
    (match-define (md:footnote-definition blocks label) defn)
    (footnote-collect-element label (render-flow blocks))))

(define (make-part-info #:title title-content #:depth depth)
  (struct-copy part-start (section title-content) [depth depth]))

; Taken from scribble/decode.
(define (make-part-to-collect tag title-content)
  (list (index-element
         #f '() tag
         (list (clean-up-index-string
                (regexp-replace #px"^\\s+(?:(?:A|An|The)\\s)?"
                                (content->string title-content) "")))
         (list (element #f title-content))
         (make-part-index-desc))))
