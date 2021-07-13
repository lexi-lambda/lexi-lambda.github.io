#lang racket/base

(require racket/contract
         racket/match
         scribble/base
         scribble/core
         scribble/decode
         scribble/decode-struct
         "../scribble/post-language.rkt"
         (prefix-in md: "parse.rkt"))

(provide (contract-out
          [document->part (-> md:document? content? part?)]))

(define current-link-targets (make-parameter #f))
(define current-footnotes (make-parameter #f))

(define (document->part doc title-content)
  (parameterize ([current-link-targets (md:document-link-targets doc)]
                 [current-footnotes (footnotes->elements (md:document-footnotes doc))])
    (match-define-values [main-part '()] (blocks->part (md:document-blocks doc) title-content 0))
    (define all-blocks (append (part-blocks main-part) (footnotes->blocks (md:document-footnotes doc))))
    (struct-copy part main-part [blocks all-blocks])))

(define (footnotes->elements notes)
  (for/hash ([note (in-list notes)])
    (match-define (cons name _) note)
    (values name (footnote-reference-element (string->symbol name)))))

(define (footnotes->blocks notes)
  (for/list ([note (in-list notes)])
    (match-define (cons name md-blocks) note)
    (footnote-flow (string->symbol name) (blocks->blocks md-blocks))))

(define (blocks->part md-blocks title-content part-depth)
  (define (collect-blocks blocks md-blocks)
    (match md-blocks
      ['() (finish-part (reverse blocks) '() '())]
      [(cons (md:heading depth _) _)
       (if (> depth part-depth)
           (collect-parts (reverse blocks) '() md-blocks)
           (finish-part (reverse blocks) '() md-blocks))]
      [(cons md-block md-blocks)
       (collect-blocks (cons (block->block md-block) blocks) md-blocks)]))

  (define (collect-parts blocks parts md-blocks)
    (match md-blocks
      [(cons (md:heading depth md-content) md-blocks)
       #:when (> depth part-depth)
       (define-values [part md-blocks*] (blocks->part md-blocks
                                                      (content->content md-content)
                                                      (add1 part-depth)))
       (collect-parts blocks (cons part parts) md-blocks*)]
      [_ (finish-part blocks (reverse parts) md-blocks)]))

  (define (finish-part blocks parts md-blocks)
    (define part-tag `(part ,(gen-tag title-content)))
    (values (part #f
                  (list part-tag)
                  title-content
                  plain
                  (make-part-to-collect part-tag title-content)
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
    [(md:ordered-list md-blockss)
     (itemization (style 'ordered '()) (map blocks->blocks md-blockss))]
    [(md:blockquote md-blocks)
     (nested-flow (style 'nested '()) (blocks->blocks md-blocks))]))

(define (blocks->blocks md-blocks)
  (map block->block md-blocks))

(define (content->content md-content)
  (match md-content
    [(? string?) md-content]
    [(? list?) (map content->content md-content)]
    [(md:bold md-content) (element 'bold (content->content md-content))]
    [(md:italic md-content) (element 'italic (content->content md-content))]
    [(md:code md-content) (code (content->content md-content))]
    [(md:link md-content dest) (hyperlink (destination->string dest) (content->content md-content))]
    [(md:image md-content dest) (image (destination->string dest) (content->content md-content))]
    [(md:footnote-ref name) (hash-ref (current-footnotes) name)]))

(define (destination->string dest)
  (match dest
    [(? string?) dest]
    [(md:reference name) (hash-ref (current-link-targets) name)]))

; Taken from scribble/private/tag.
(define (gen-tag content)
  (regexp-replace* #px"[^-a-zA-Z0-9_=\u4e00-\u9fff\u3040-\u309F\u30A0-\u30FF]"
                   (content->string content) "_"))

; Taken from scribble/decode.
(define (make-part-to-collect tag title-content)
  (list (index-element
         #f '() tag
         (list (clean-up-index-string
                (regexp-replace #px"^\\s+(?:(?:A|An|The)\\s)?"
                                (content->string title-content) "")))
         (list (element #f title-content))
         (make-part-index-desc))))
