#lang racket/base

(require racket/contract
         racket/format
         racket/match
         racket/string
         scribble/core
         scribble/private/literal-anchor
         syntax/parse/define)

(provide when/list
         unless/list
         cond/list
         (contract-out
          [to-slug (-> string? string?)]

          [normalize-element-style (-> element-style? style?)]
          [tag->anchor-name (-> tag? string?)]))

;; -----------------------------------------------------------------------------

(define (to-slug s)
  (define rx #px"[^a-z0-9]+")
  (regexp-replace* rx (string-trim (string-downcase s) rx) "-"))

;; -----------------------------------------------------------------------------
;; xexprs

(define-simple-macro (when/list condition:expr body ...+)
  (if condition (list (let () body ...)) '()))
(define-simple-macro (unless/list condition:expr body ...+)
  (if condition '() (list (let () body ...))))
(define-simple-macro (cond/list clause ...)
  (cond clause ... [else '()]))

;; -----------------------------------------------------------------------------
;; scribble

(define (normalize-element-style v)
  (if (style? v) v (style v '())))

(define tag->anchor-name
  (match-lambda
    [(literal-anchor anchor-name) anchor-name]
    [(cons 'part tag) (tag->anchor-name tag)]
    [(? list? elements)
     ; This anchor naming scheme does not in any way create unique anchors, but
     ; that should be okay for internal references in this use case, and having
     ; pretty URLs is a nice feature.
     (to-slug (string-join (map ~a elements) " "))]))
