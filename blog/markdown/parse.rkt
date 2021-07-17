#lang racket/base

(require data/applicative
         data/monad
         megaparsack
         megaparsack/text
         racket/contract
         racket/function
         racket/list
         racket/match
         threading
         xml
         "parse/content.rkt"
         "parse/util.rkt")

(provide (all-from-out "parse/content.rkt")

         block?
         horizontal-rule
         horizontal-rule?
         (contract-out
          (struct document ([blocks (listof block?)]
                            [link-targets (hash/c string? string?)]
                            [footnotes (listof (cons/c string? (listof block?)))]))
          (struct heading ([depth exact-nonnegative-integer?] [content content?]))
          (struct paragraph ([content content?]))
          (struct code-block ([content content?] [language (or/c #f string?)]))
          (struct unordered-list ([blockss (listof (listof block?))]))
          (struct ordered-list ([blockss (listof (listof block?))]))
          (struct blockquote ([blocks (listof block?)]))
          (struct html-block ([xexpr xexpr/c]))

          [document/p (parser/c char? document?)]))

;; -----------------------------------------------------------------------------

(struct document (blocks link-targets footnotes) #:transparent)
(struct heading (depth content) #:transparent)
(struct paragraph (content) #:transparent)
(struct code-block (content language) #:transparent)
(struct unordered-list (blockss) #:transparent)
(struct ordered-list (blockss) #:transparent)
(struct blockquote (blocks) #:transparent)
(struct html-block (xexpr) #:transparent)
(define-values [horizontal-rule horizontal-rule?]
  (let ()
    (struct horizontal-rule ())
    (values (horizontal-rule) horizontal-rule?)))

(define (block? v)
  (or (heading? v)
      (paragraph? v)
      (code-block? v)
      (unordered-list? v)
      (ordered-list? v)
      (blockquote? v)
      (html-block? v)
      (horizontal-rule? v)))

;; -----------------------------------------------------------------------------

(define (spaces/p n) (repeat/p n (char/p #\space)))
(define maybe-indent/p (map/p length (many/p (char/p #\space))))

(define unordered-list-bullet/p (or/p (string/p "* ") (string/p "- ")))
(define ordered-list-bullet/p (do integer/p (string/p ". ")))

;; -----------------------------------------------------------------------------

#| This module implements a simple markdown parser based very loosely on the
algorithm given in the CommonMark specification:

    https://spec.commonmark.org/0.30/#appendix-a-parsing-strategy

This implementation is very much /not/ CommonMark-compliant. For the most part,
it is stricter than CommonMark---not all inputs are accepted as valid markdown.
The accepted subset is chosen to be both simple to parse and relatively portable
with other markdown implementations.

Although the parser is implemented using megaparsack, it is unusual in that it
is really a one-line-at-a-time parser that switches between various states. This
makes it easier to parse Markdown’s indentation-sensitive block structure.

Note [Open blocks]
~~~~~~~~~~~~~~~~~~
Philosophically, the parser is a context-sensitive state machine. Some example
states are “start of line”, “scanning block prefixes”, and “scanning content”.
Each state is implemented as a parser-returning function that accepts its
context as arguments. Different states accept the context in slightly different
ways, but in all cases, the parsing context is a stack of /open blocks/, as
described in the CommonMark appendix linked above.

Internally, the parser represents most open blocks using the same structures it
uses to represent closed blocks (the latter of which are recognized by the
public `block?` predicate), but there are two differences:

  * Open lists are represented by the `open-list` and `list-element` structures,
    which record additional indentation information.

  * Open footnote declarations are represented by the `footnote` structure.
    These never appear in closed blocks, since they are collected into a field
    of the top-level `document` structure, but they can appear nested in other
    blocks, so parsing them must still be indentation-aware.

Note [In-blocks and out-blocks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The naming conventions in this module distinguish two forms of block context:

  * When a function accepts a list of open blocks /shallowest-first/, the
    argument is named `out-blocks`. This is because it’s useful to access
    shallower blocks before deeper blocks when the parser is “outside” the blocks.

  * Dually, when a function accepts a list of open blocks /deepest-first/, the
    argument is named `in-blocks`.

As an example, the `scan-line-start` function accepts an `out-blocks` argument
because it must traverse the open blocks from the outside in to parse the
indentation at the start of the line. In contrast, the `scan-block-content`
function accepts an `in-blocks` argument, since the indentation has been parsed
for all open blocks, and the content should be added to the innermost block.

The `scan-block-prefixes` function bridges the gap between `scan-line-start` and
`scan-block-content`, so it accepts both `in-blocks` and `out-blocks` arguments.
As it parses the indentation for each block, it transfers it from the
`out-blocks` stack to the `in-blocks` stack, morally “entering” the block. |#

; see Note [Open blocks]
(struct open-list (type indent blockss) #:transparent)
(struct list-element (indent blocks) #:transparent)
(struct footnote (name blocks) #:transparent)

(define (leaf-block? block)
  (or (paragraph? block)
      (code-block? block)
      (heading? block)
      (horizontal-rule? block)))

(define (close-block block)
  (match block
    [(open-list 'unordered _ blockss) (unordered-list blockss)]
    [(open-list 'ordered _ blockss)   (ordered-list blockss)]
    [(paragraph content)              (paragraph (simplify-content content))]
    [_                                block]))

; Closes a sequence of open blocks. Returns two values:
;   1. a list of (closed) blocks
;   2. a list of footnotes declared in the given blocks
(define (close-blocks out-blocks)
  (for/foldr ([blocks '()] [notes '()])
             ([out-block (in-list out-blocks)])
    (match out-block
      [(? leaf-block?)
       (values (list (close-block out-block)) notes)]
      [(footnote name old-blocks)
       (values '() (cons (cons name (append old-blocks blocks)) notes))]
      [_
       (values (list (close-block (add-blocks out-block blocks))) notes)])))

(define (add-blocks open-block new-blocks)
  (match open-block
    [(document blocks targets notes) (document (append blocks new-blocks) targets notes)]
    [(open-list type indent blockss) (open-list type indent (append blockss (map list-element-blocks new-blocks)))]
    [(list-element indent blocks)    (list-element indent (append blocks new-blocks))]
    [(blockquote blocks)             (blockquote (append blocks new-blocks))]
    [(footnote name blocks)          (footnote name (append blocks new-blocks))]))

(define (add-footnotes doc new-notes)
  (define notes (append (document-footnotes doc) new-notes))
  (struct-copy document doc [footnotes notes]))

(define (add-footnotes-to-out-blocks out-blocks new-notes)
  (cons (add-footnotes (first out-blocks) new-notes) (rest out-blocks)))

(define (add-footnotes-to-in-blocks in-blocks new-notes)
  (if (empty? new-notes)
      in-blocks
      (reverse (add-footnotes-to-out-blocks (reverse in-blocks) new-notes))))

; Handles the case where we reach the end of a line before we’ve seen enough
; indentation to enter all open blocks. Any blockquotes we aren’t already
; inside must be closed, since we didn’t see the `>` yet. Also, a paragraph is
; necessarily closed by a blank line.
(define (finish-blank-line in-blocks out-blocks)
  (define-values [in-blocks* out-blocks*]
    (splitf-at out-blocks (negate (disjoin paragraph? blockquote?))))
  (match-define (cons inner-block other-blocks) (append (reverse in-blocks*) in-blocks))
  (define-values [closed-blocks notes] (close-blocks out-blocks*))
  (~> (reverse (cons (add-blocks inner-block closed-blocks) other-blocks))
      (add-footnotes-to-out-blocks notes)))

;; -----------------------------------------------------------------------------
;; the parser

(define document/p
  (lazy/p (scan-line-start (list (document '() (hash) '())))))

(define (scan-line-start out-blocks)
  (or/p (do eof/p
            (match-define-values [(list doc) notes] (close-blocks out-blocks))
            (pure (add-footnotes doc notes)))
        (lazy/p (scan-block-prefixes '() out-blocks))))

(define (scan-block-prefixes in-blocks out-blocks) ; see Note [In-blocks and out-blocks]
  (match out-blocks
    [(cons out-block out-blocks*)
     (or/p
      ; Case 1: We’re definitely still in this block. (Well, mostly.
      ;   See Note [Unconditionally parse list elements] for an example
      ;   where we backtrack past `scan-block-prefix`.)
      (do (scan-block-prefix out-block)
          (scan-block-prefixes (cons out-block in-blocks) out-blocks*))

      ; Case 2: End of the (blank) line. We’re done.
      (do newline/p
          (scan-line-start (finish-blank-line in-blocks out-blocks)))

      ; Case 3: New content, but after some closed blocks.
      (lazy/p (do (define-values [closed-blocks notes] (close-blocks out-blocks))
                  (match (add-footnotes-to-in-blocks in-blocks notes)
                    [(cons inner-block other-blocks)
                     (scan-block-content (add-blocks inner-block closed-blocks) other-blocks)]
                    ['() (fail/p (message (srcloc #f #f #f #f #f) "" '()))]))))]
    ['()
     ; Case 4: Still inside every block, but there might be new content.
     (do (match-define (cons inner-block other-blocks) in-blocks)
         (scan-block-content inner-block other-blocks))]))

(define (scan-block-prefix out-block)
  (do (try/p (do (match out-block
                   ; Paragraphs must have actual content to enter them, since
                   ; blank lines end paragraphs.
                   [(? paragraph?)          (lookahead/p (char-not-in/p "\r\n"))]
                   [(open-list _ indent _)  (spaces/p indent)]
                   [(list-element indent _) (spaces/p indent)]
                   [(? blockquote?)         (char/p #\>)]
                   [(? footnote?)           (spaces/p 4)]
                   [_                       void/p])))
      (match out-block
        [(? blockquote?)
         ; If this is a blockquote, we need to consume an additional space, but
         ; it can be omitted if we’re at the end of a line.
         (or/p (lookahead/p newline/p)
               (char/p #\space))]
        [_ void/p])))

(define (scan-block-content inner-block in-blocks)
  (match inner-block
    ; If we’re in a paragraph, just append to its content.
    [(paragraph content)
     (do [new-content <- (many/p content/p)]
         newline/p
         (scan-line-start (reverse (cons (paragraph (list content new-content "\n")) in-blocks))))]

    ; If we’re in a codeblock, check for an end marker, otherwise append to its content.
    [(code-block content language)
     (or/p (do (try/p (string/p "```"))
               newline/p
               (scan-line-start (reverse (cons (add-blocks (first in-blocks) (list inner-block)) (rest in-blocks)))))
           (do [new-content <- rest-of-line/p]
               (scan-line-start (reverse (cons (code-block (string-append content new-content "\n") language) in-blocks)))))]

    ; If we’re in a list (but not a list element), parse a new list element.
    ; See Note [Unconditionally parse list elements] for why we don’t have to handle anything else.
    [(open-list 'unordered _ _)
     (do unordered-list-bullet/p
         (scan-block-content (list-element 2 '()) (cons inner-block in-blocks)))]
    [(open-list 'ordered _ _)
     (do [bullet <- (syntax-box/p ordered-list-bullet/p)]
         (scan-block-content (list-element (srcloc-span (syntax-box-srcloc bullet)) '())
                             (cons inner-block in-blocks)))]

    ; Otherwise, we’re in a container block, so try to start a new block.
    [_ (scan-new-block inner-block in-blocks)]))

(define (scan-new-block inner-block in-blocks)
  (or/p (do newline/p
            (scan-line-start (reverse (cons inner-block in-blocks))))

        ; horizontal rules
        (do (try/p (string/p "---"))
            (many/p (char/p #\-))
            newline/p
            (scan-line-start (reverse (cons (add-blocks inner-block (list horizontal-rule)) in-blocks))))

        ; headings
        (do [depth <- (try/p (do [depth-chars <- (many+/p (char/p #\#))]
                                 space/p
                                 (pure (length depth-chars))))]
            [content <- (map/p simplify-content (many/p content/p))]
            (scan-line-start (reverse (cons (add-blocks inner-block (list (heading depth content))) in-blocks))))

        ; footnote and link declarations
        (scan-footnote-declaration (cons inner-block in-blocks))
        (scan-link-target-declaration (cons inner-block in-blocks))

        ; blockquotes
        (do (char/p #\>)
            (or/p (lookahead/p newline/p)
                  (char/p #\space))
            (scan-block-content (blockquote '()) (cons inner-block in-blocks)))

        ; code blocks
        (do (try/p (string/p "```"))
            [language <- (or/p (do newline/p (pure #f))
                               rest-of-line/p)]
            (scan-line-start (reverse (list* (code-block "" language) inner-block in-blocks))))

        ; lists
        (scan-list-start (cons inner-block in-blocks) 'unordered unordered-list-bullet/p)
        (scan-list-start (cons inner-block in-blocks) 'ordered ordered-list-bullet/p)

        ; html blocks
        (do (lookahead/p (or/p (try/p (string/p "<div"))
                               (try/p (string/p "<h2"))))
            [html-str <- rest-of-line/p]
            (scan-line-start (reverse (cons (add-blocks inner-block (list (html-block (string->xexpr html-str)))) in-blocks))))

        ; free-form content (new paragraph)
        (scan-block-content (paragraph '()) (cons inner-block in-blocks))))

(define (scan-link-target-declaration in-blocks)
  (do [name <- (try/p (do [name <- (around-string/p (char/p #\[) (char/p #\]))]
                          (char/p #\:)
                          (pure name)))]
      (many/p (char/p #\space))
      [dest <- rest-of-line/p]
      (match-define (cons (document blocks targets notes) out-blocks) (reverse in-blocks))
      (scan-line-start (cons (document blocks (hash-set targets name dest) notes) out-blocks))))

(define (scan-footnote-declaration in-blocks)
  (do [name <- (try/p (do [name <- (around-string/p (string/p "[^") (char/p #\]))]
                          (char/p #\:)
                          (pure name)))]
      (many/p (char/p #\space))
      (scan-block-content (footnote name '()) in-blocks)))

(define (scan-list-start in-blocks type start-p)
  (do [indent <- (try/p (do [indent <- maybe-indent/p]
                            (lookahead/p start-p)
                            (pure indent)))]
      (scan-block-content (open-list type indent '()) in-blocks)))

#| Note [Unconditionally parse list elements]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a markdown document that looks like this:

  1 |* foo
  2 |
  3 |bar

The correct parse here is clearly an unordered list with a single element,
followed by a paragraph. However, because the list is unindented, we will end up
in a parsing context that looks like this at the beginning of line 3:

  location: 3 |bar
              ^
  state: scan-block-content
  in blocks: document
  inner block: open-list['unordered 0]
               └ paragraph{foo}

At first blush, this is a problem, because in `scan-block-content`, we
/unconditionally/ parse a list element if the innermost block is an `open-list`.
Since line 3 does not start with a `*` or `-`, the parser fails.

However, note that the parser fails /without consuming input/. It is not
immediately obvious that this helps---calls to `scan-block-content` are usually
placed as the /last/ alternative of an `or/p` sequence---but in fact this
failure allows backtracking to an even earlier choice point. Specifically, it
can backtrack all the way to this state:

  location: 3 |bar
              ^
  state: scan-block-prefixes
  in blocks: document
  out blocks: open-list['unordered 0]
              list-element[2]
              └ paragraph{foo}

Note, crucially, that we’ve backtracked far enough that the `open-list` is still
in `out-blocks`, not `in-blocks`. This is somewhat surprising, because
`scan-block-prefix` will always succeed on an unindented `open-list`,
immediately moving it into `in-blocks`. However, since `scan-block-prefix` does
not consume any input in that case, the parser has not yet committed to entering
that block.

This subtlety means we do not need to treat this case specially in
`scan-block-content`: we can just unconditionally parse a list element when
inside an `open-list`. And intuitively, this makes some sense given
megaparsack’s backtracking behavior: the parser only commits when consuming
input, and at the start of the line, no input has been consumed. |#
