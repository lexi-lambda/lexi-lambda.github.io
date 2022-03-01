#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/contract
         racket/format
         racket/list
         racket/match
         scribble/core
         scribble/decode
         scribble/html-properties
         syntax/parse/define
         threading

         "metadata.rkt")

(provide define-footnote
         (contract-out
          [make-footnote-tag (->* [string?]
                                  [#:post (or/c string? #f)
                                   #:tag-prefixes (listof string?)]
                                  tag?)]
          [footnote-ref (-> string? content?)]
          [footnote-link (-> string? pre-content? ... link-element?)]
          [footnote-decl (-> string? pre-flow? ... part-collect-decl?)]
          [footnote-collect-element (-> string? (listof block?) content?)]
          [footnotes-section (-> part?)]))

(define (make-footnote-tag tag-str
                           #:post [post-path #f]
                           #:tag-prefixes [tag-prefixes '()])
  `(footnote ,(taglet-add-prefix
               (cons 'prefixable
                     (if post-path
                         (cons (blog-post-path->tag-prefix post-path) tag-prefixes)
                         tag-prefixes))
               tag-str)))

(define footnote-refs-key (gensym 'footnote-refs))
(define footnote-ref-targets-key (gensym 'footnote-ref-targets))
(define footnote-flows-key (gensym 'footnote-flows))

(define (footnote-ref tag)
  (traverse-element
   (λ (get set)
     (define refs (get footnote-refs-key '()))
     (define number (cond
                      [(index-of refs tag)
                       => (λ (idx) (- (length refs) idx))]
                      [else
                       (set footnote-refs-key (cons tag refs))
                       (add1 (length refs))]))

     (define all-ref-targets (get footnote-ref-targets-key (hash)))
     (define ref-targets (hash-ref all-ref-targets tag '()))
     (define ref-target `(footnote-ref (prefixable ,tag ,(~a (add1 (length ref-targets))))))
     (set footnote-ref-targets-key
          (hash-set all-ref-targets
                    tag
                    (cons ref-target ref-targets)))

     (link-element (style 'superscript (list (link-target ref-target)))
                   (~a number)
                   (make-footnote-tag tag)))))

(define (footnote-link tag . pre-content)
  (link-element #f (decode-content pre-content) (make-footnote-tag tag)))

(define (footnote-decl tag . pre-flows)
  (part-collect-decl
   (element #f (footnote-collect-element tag (decode-flow pre-flows)))))

(define (footnote-collect-element tag blocks)
  (traverse-element
   (λ (get set)
     (set footnote-flows-key
          (cons (cons tag blocks)
                (get footnote-flows-key '())))
     '())))

(define (footnotes-section)
  (define (generate get set)
    (define refs (get footnote-refs-key '()))
    (define ref-targets (get footnote-ref-targets-key (hash)))
    (define-values [referenced-flows other-flows]
      (partition (λ~> car (member refs)) (get footnote-flows-key '())))

    ; Tacks the ↩ “return to reference” links onto the end of the footnote’s
    ; flow, folding them into a trailing paragraph if at all possible.
    (define (add-ref-links blocks ref-tags)
      (cond
        [(empty? ref-tags) blocks]
        [else
         (define ref-links (~> (for/list ([ref-tag (in-list ref-tags)])
                                 (link-element #f "↩" ref-tag))
                               (add-between " ")))
         (reverse
          (match (reverse blocks)
            [(cons (paragraph p-style p-content) blocks)
             (cons (paragraph p-style (list* p-content " " ref-links)) blocks)]
            [blocks
             (cons (paragraph plain ref-links) blocks)]))]))

    (define (build-item number tag+blocks)
      (match-define (cons tag blocks) tag+blocks)
      (nested-flow
       (style #f (list* (alt-tag "li")
                        (link-target (make-footnote-tag tag))
                        (if number
                            (list (attributes `([value . ,(~a number)])))
                            '())))
       (add-ref-links blocks (reverse (hash-ref ref-targets tag '())))))

    ; Build the footnote items, starting with the ones that were actually
    ; referenced. If a footnote was referenced that doesn’t actually exist, we
    ; want to skip its number in the output list, which requires we set the
    ; `value` attribute on the generated `li`. The `skipped?` argument tracks
    ; whether or not this is necessary for the next `li`.
    (define items
      (let loop ([refs (reverse refs)]
                 [number 1]
                 [skipped? #f])
        (match refs
          [(cons ref refs)
           (match (assoc ref referenced-flows)
             [(? pair? flow)
              (cons (build-item (and skipped? number) flow)
                    (loop refs (add1 number) #f))]
             [#f (loop refs (add1 number) #t)])]

          ; We’re done outputting the referenced footnotes. If there were any
          ; unreferenced ones, just tack them onto the end.
          ['() (match other-flows
                 ['() '()]
                 [(cons flow flows)
                  (cons (build-item (and skipped? number) flow)
                        (map (λ~>> (build-item #f)) flows))])])))

    (nested-flow (style #f (list (alt-tag "ol")
                                 (attributes '([class . "footnotes"]))))
                 items))
  (part #f
        (list `(part ,(generated-tag)))
        #f
        (style #f '(unnumbered hidden toc-hidden))
        '()
        (list (traverse-block generate))
        '()))

(define-syntax-parser define-footnote
  [(_ ref-id:id {~optional {~seq #:tag tag-e}} pre-flow ...)
   #:declare tag-e (expr/c #'string? #:name "tag")
   #:with note-ref-id (format-id #'ref-id "note:~a" #'ref-id #:subs? #t)
   #:with note-link-id (format-id #'ref-id "note:~a*" #'ref-id #:subs? #t)
   #`(begin
       (define note-tag {~? tag-e.c '#,(symbol->string (syntax-e #'ref-id))})
       (define (note-ref-id) (footnote-ref note-tag))
       (define (note-link-id . pre-content)
         (apply footnote-link note-tag pre-content))
       (footnote-decl note-tag pre-flow ...))])
