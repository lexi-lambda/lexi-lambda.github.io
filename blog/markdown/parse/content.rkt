#lang racket/base

(require data/applicative
         data/monad
         megaparsack
         megaparsack/text
         racket/contract
         racket/match
         racket/string
         threading
         "util.rkt")

(provide (contract-out
          [content? predicate/c]
          (struct bold ([content content?]))
          (struct italic ([content content?]))
          (struct code ([content content?]))
          (struct link ([content content?] [dest destination?]))
          (struct image ([alt-text string?] [dest destination?]))
          (struct footnote-ref ([name string?]))

          [destination? predicate/c]
          (struct reference ([name string?]))

          [content/p (parser/c char? content?)]
          [simplify-content (-> content? content?)]))

;; -----------------------------------------------------------------------------

(struct bold (content) #:transparent)
(struct italic (content) #:transparent)
(struct code (content) #:transparent)
(struct link (content dest) #:transparent)
(struct image (alt-text dest) #:transparent)
(struct footnote-ref (name) #:transparent)

(define (content? v)
  (or (string? v)
      (bold? v)
      (italic? v)
      (code? v)
      (link? v)
      (image? v)
      (footnote-ref? v)
      (and (list? v) (andmap content? v))))

(struct reference (name) #:transparent)

(define (destination? v)
  (or (string? v)
      (reference? v)))

;; -----------------------------------------------------------------------------

(define content/p
  (label/p "content"
    (or/p (map/p list->string (many+/p (char-not-in/p "*_`[]<!\\\r\n")))
          ; bold / italic
          (map/p (λ~> simplify-content bold) (around/p (try/p (string/p "**")) (lazy/p content/p)))
          (map/p (λ~> simplify-content italic) (around/p (char/p #\*) (lazy/p content/p)))
          ; code
          (do [backticks <- (many+/p (char/p #\`))]
              [chars <- (many-till/p any-char/p (try/p (string/p (list->string backticks))))]
              (pure (code (string-trim (list->string chars)))))
          ; footnote ref
          (map/p footnote-ref (around-string/p (try/p (string/p "[^")) (char/p #\])))
          ; link
          (do [content <- (around/p (char/p #\[) (lazy/p content/p) (char/p #\]))]
              (or/p (do [dest <- destination/p]
                        (pure (link (simplify-content content) dest)))
                    (pure (simplify-content (list "[" content "]")))))
          ; image
          (do (try/p (string/p "!["))
              [alt-text <- (map/p list->string (many-till/p any-char/p (char/p #\])))]
              [dest <- destination/p]
              (pure (image alt-text dest)))
          ; special char
          (map/p string (char-in/p "!]")))))

(define (simplify-content content)
  (match content
    [(list content)          (simplify-content content)]
    [(cons content contents) (simplify-content-list content contents)]
    [_                       content]))

(define (simplify-content-list content contents)
  (match content
    ['() (simplify-content contents)]
    [(cons content contents*)
     (simplify-content-list content (cons contents* contents))]
    [(? string?)
     (match (simplify-content contents)
       [(list* (? string? strs) ..1 contents)
        (cons (string-append* content strs) contents)]
       [(? string? str)    (string-append content str)]
       [(? list? contents) (cons content contents)]
       [content*           (list content content*)])]
    [_
     (match (simplify-content contents)
       [(? list? contents) (cons content contents)]
       [content*           (list content content*)])]))

(define destination/p
  (label/p "link destination"
    (or/p (around-string/p (char/p #\() (char/p #\)))
          (map/p reference (around-string/p (char/p #\[) (char/p #\]))))))
