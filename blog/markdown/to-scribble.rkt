#lang racket/base

(require racket/contract
         racket/format
         racket/list
         racket/match
         scribble/base
         scribble/core
         scribble/decode
         scribble/decode-struct
         scribble/html-properties

         (only-in "../lang/post-language.rkt"
                  code
                  code-block
                  footnote-collect-element
                  footnote-ref
                  footnotes-section
                  pygments-block)
         (prefix-in md: "parse.rkt"))

(provide (contract-out
          [document->part (-> md:document? title-decl? part?)]
          [content->content (-> md:content? content?)]))

(define current-link-targets (make-parameter #f))
(define current-footnotes (make-parameter #f))

(define (document->part doc title-decl)
  (parameterize ([current-link-targets (md:document-link-targets doc)]
                 [current-footnotes (footnotes->reference-elements (md:document-footnotes doc))])
    (define part-info (part-start 0
                                  (title-decl-tag-prefix title-decl)
                                  (title-decl-tags title-decl)
                                  (title-decl-style title-decl)
                                  (title-decl-content title-decl)))
    (match-define-values [main-part '()] (blocks->part (md:document-blocks doc) part-info))
    (struct-copy part main-part
                 [to-collect (append (footnotes->decl-elements (md:document-footnotes doc))
                                     (part-to-collect main-part))]
                 [parts (append (part-parts main-part) (list (footnotes-section)))])))

(define (footnotes->reference-elements notes)
  (for/hash ([note (in-list notes)])
    (match-define (cons name _) note)
    (values name (footnote-ref name))))

(define (footnotes->decl-elements notes)
  (for/list ([note (in-list notes)])
    (match-define (cons name md-blocks) note)
    (footnote-collect-element name (blocks->blocks md-blocks))))

(define (blocks->part md-blocks part-info)
  (define (collect-blocks blocks md-blocks)
    (match md-blocks
      ['() (finish-part (reverse blocks) '() '())]
      [(cons (md:heading depth _) _)
       (if (> depth (part-start-depth part-info))
           (collect-parts (reverse blocks) '() md-blocks)
           (finish-part (reverse blocks) '() md-blocks))]
      [(cons md-block md-blocks)
       (collect-blocks (cons (block->block md-block) blocks) md-blocks)]))

  (define (collect-parts blocks parts md-blocks)
    (match md-blocks
      [(cons (md:heading depth md-content) md-blocks)
       #:when (> depth (part-start-depth part-info))
       (define-values [part md-blocks*]
         (blocks->part md-blocks
                       (make-part-info #:title (content->content md-content)
                                       #:depth (add1 (part-start-depth part-info)))))
       (collect-parts blocks (cons part parts) md-blocks*)]
      [_ (finish-part blocks (reverse parts) md-blocks)]))

  (define (finish-part blocks parts md-blocks)
    (define part-tags (if (empty? (part-start-tags part-info))
                          (list `(part ,(make-generated-tag)))
                          (part-start-tags part-info)))
    (define title-content (part-start-title part-info))
    (values (part (part-start-tag-prefix part-info)
                  part-tags
                  title-content
                  (part-start-style part-info)
                  (make-part-to-collect (first part-tags) title-content)
                  blocks
                  parts)
            md-blocks))

  (collect-blocks '() md-blocks))

(define (block->block md-block)
  (match md-block
    [(md:paragraph content)
     (paragraph plain (content->content content))]
    [(md:code-block content language)
     (if language
         (pygments-block content #:language language)
         (code-block content))]
    [(md:unordered-list md-blockss)
     (itemization plain (map blocks->blocks md-blockss))]
    [(md:ordered-list start md-blockss)
     (define props (if (= start 1) '()
                       (list (attributes (list (cons 'start (~a start)))))))
     (itemization (style 'ordered props) (map blocks->blocks md-blockss))]
    [(md:blockquote md-blocks)
     (nested-flow (style 'nested '()) (blocks->blocks md-blocks))]
    [(? md:horizontal-rule?)
     (paragraph (style #f (list (alt-tag "hr"))) '())]
    [(md:html-block xexpr)
     (xexpr->block xexpr)]))

(define (blocks->blocks md-blocks)
  (map block->block md-blocks))

(define (content->content md-content)
  (match md-content
    [(? string?) md-content]
    [(? list?) (map content->content md-content)]
    [(md:bold md-content) (element 'bold (content->content md-content))]
    [(md:italic md-content) (element 'italic (content->content md-content))]
    [(md:code md-content) (code (content->content md-content))]
    [(md:link md-content dest)
     (define content (content->content md-content))
     (hyperlink (destination->string dest content) content)]
    [(md:image md-content dest)
     (define content (content->content md-content))
     (image (destination->string dest content) content)]
    [(md:superscript md-content) (element 'superscript (content->content md-content))]
    [(md:footnote-ref name) (hash-ref (current-footnotes) name)]))

(define (destination->string dest content)
  (match dest
    [(? string?) dest]
    [(md:reference "") (hash-ref (current-link-targets) (content->string content))]
    [(md:reference name) (hash-ref (current-link-targets) name)]))

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
