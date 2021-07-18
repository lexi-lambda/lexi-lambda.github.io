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
          [footnote-decl (-> string? pre-flow? ... part-collect-decl?)]
          [footnotes-section (-> part?)]))

(define (make-footnote-tag tag-str
                           #:post [post-path #f]
                           #:tag-prefixes [tag-prefixes '()])
  `(footnote ,(taglet-add-prefix (cons 'prefixable
                                       (if post-path
                                           (cons post-path tag-prefixes)
                                           tag-prefixes))
                                 tag-str)))

(define footnote-refs-key (gensym 'footnote-refs))
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
     (link-element (style 'superscript '())
                   (~a number)
                   (make-footnote-tag tag)))))

(define (footnote-decl tag . pre-flows)
  (part-collect-decl
   (element
    #f
    (traverse-element
     (λ (get set)
       (set footnote-flows-key
            (cons (cons tag (decode-flow pre-flows))
                  (get footnote-flows-key '())))
       '())))))

(define (footnotes-section)
  (define (generate get set)
    (define refs (get footnote-refs-key '()))
    (define-values [referenced-flows other-flows]
      (partition (λ~> car (member refs)) (get footnote-flows-key '())))

    (define (build-item number tag+blocks)
      (nested-flow
       (style #f (list* (alt-tag "li")
                        (link-target (make-footnote-tag (car tag+blocks)))
                        (if number
                            (list (attributes `([value . ,(~a number)])))
                            '())))
       (cdr tag+blocks)))

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
   #`(begin
       (define note-tag {~? tag-e.c '#,(symbol->string (syntax-e #'ref-id))})
       (define (note-ref-id) (footnote-ref note-tag))
       (footnote-decl note-tag pre-flow ...))])
