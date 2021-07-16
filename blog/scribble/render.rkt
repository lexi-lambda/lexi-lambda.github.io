#lang racket/base

(require (prefix-in scribble: scribble/html-render)
         (prefix-in xml: xml)

         racket/class
         racket/format
         racket/hash
         racket/list
         racket/match
         racket/path
         racket/serialize
         racket/string

         scribble/base-render
         scribble/core
         scribble/html-properties
         scribble/private/literal-anchor
         (only-in scribble/private/render-utils part-style?)

         net/uri-codec
         net/url
         setup/dirs
         threading

         "../paths.rkt"
         "../util.rkt"
         "../highlight/pygments.rkt"
         "post-language.rkt"
         "render/template.rkt")

(provide rendered-post?
         render-mixin
         render-post-page
         render-post-index
         tag->anchor-name)

;; -----------------------------------------------------------------------------

(serializable-struct rendered-post (title-str title date tags body) #:transparent)

;; -----------------------------------------------------------------------------

(define (normalize-style v)
  (if (style? v) v (style v '())))

(define (attributes-union attrs1 attrs2)
  (hash-union attrs1 attrs2 #:combine/key
              (λ (k a b)
                (match k
                  ['class (string-append a " " b)]
                  [_ (error 'merge-attributes
                            (string-append "duplicate values for attribute\n"
                                           "  attribute: ~e\n"
                                           "  values:\n"
                                           "   ~e\n"
                                           "   ~e")
                            k a b)]))))

(define (attributes->list attrs)
  (for/list ([(k v) (in-hash attrs)])
    (list k v)))

(define (style->tag-name+attributes s)
  (for/fold ([tag-name #f]
             [attrs (if (string? (style-name s))
                        (hasheq 'class (style-name s))
                        (hasheq))])
            ([prop (in-list (style-properties s))])
    (match prop
      ['div
       (values 'div attrs)]
      [(alt-tag name)
       (values (string->symbol name) attrs)]
      [(attributes attrs*)
       (values tag-name (attributes-union attrs (make-immutable-hasheq attrs*)))]
      [_
       (values tag-name attrs)])))

(define (resolve-tag base-tag ri)
  (add-current-tag-prefix (tag-key base-tag ri)))

(define tag->anchor-name
  (match-lambda
    [(literal-anchor anchor-name) anchor-name]
    [(? list? elements)
     ; This anchor naming scheme does not in any way create unique anchors, but that should be okay
     ; for internal references in this use case, and having pretty URLs is a nice feature.
     (~> elements
         (map ~a _)
         (string-join "-")
         string-downcase
         (regexp-replace* #px"(?:[^a-z0-9])+" _ "-"))]))

(define tag->local-redirect-query-string
  (let ()
    (define racket-renderer (new (scribble:render-mixin render%)
                                 [dest-dir (find-system-path 'temp-dir)]))
    (with-method ([tag->query-string {racket-renderer tag->query-string}])
      (λ (tag) (tag->query-string tag)))))

; values associated with tags during the collect pass
(struct blog-post (title path) #:prefab)
(struct blog-post-anchor (title path anchor) #:prefab)

(define header-depth->html-tag
  (match-lambda
    [0 'h1]
    [1 'h2]
    [2 'h3]
    [3 'h4]
    [4 'h5]
    [_ 'h6]))

(define current-output-file (make-parameter #f))
(define current-top-part (make-parameter #f))

;; -----------------------------------------------------------------------------

(define (render-mixin %)
  (class %
    (define/override (current-render-mode) '(html))
    (define/override (get-suffix) #".info")

    (define external-tag-path (string->url (get-doc-search-url)))
    (define/override (set-external-tag-path p)
      (set! external-tag-path (string->url p)))

    ;; -------------------------------------------------------------------------
    ;; collect

    (inherit collect-part)

    (define footnote-ids '())

    (define/override (start-collect ds fns ci)
      (set! footnote-ids '())
      (for-each (lambda (d fn)
                  (parameterize ([current-output-file fn]
                                 [current-top-part d])
                    (collect-part d #f ci null 1 #hash())))
                ds
                fns))

    (define/public (part-whole-page? p ri)
      (match (resolve-get p ri (car (part-tags p)))
        ; racket doc reference; index 4 corresponds to `whole-page?`
        [(? vector? dest) (vector-ref dest 4)]
        ; blog references
        [(? blog-post?) #t]
        [(? blog-post-anchor?) #f]))

    (define/public (current-part-whole-page? d)
      (eq? d (current-top-part)))

    (define/override (fresh-tag-collect-context? d ci)
      (current-part-whole-page? d))
    (define/override (fresh-tag-resolve-context? d ri)
      (part-whole-page? d ri))
    (define/override (fresh-tag-render-context? d ri)
      (part-whole-page? d ri))

    (define/override (collect-part-tags d ci number)
      (for ([t (part-tags d)])
        (define key (generate-tag t ci))
        (define title (or (part-title-content d) '("???")))
        (collect-put! ci key
                      (if (current-part-whole-page? d)
                          (blog-post title (current-output-file))
                          (blog-post-anchor title
                                            (current-output-file)
                                            (tag->anchor-name (add-current-tag-prefix key)))))))

    (define/override (collect-nested-flow b ci)
      (cond
        [(findf footnote-definition? (style-properties (nested-flow-style b)))
         => (λ (defn) (set! footnote-ids (cons (footnote-definition-note-id defn) footnote-ids)))])
      (super collect-nested-flow b ci))

    (define/override (collect-target-element i ci)
      (define key (generate-tag (target-element-tag i) ci))
      (when (redirect-target-element? i)
        (raise-arguments-error 'collect-target-element "redirect targets not supported"
                               "element" i))
      (collect-put! ci key (blog-post-anchor #f
                                             (current-output-file)
                                             (tag->anchor-name (add-current-tag-prefix key)))))

    ;; -------------------------------------------------------------------------
    ;; render

    (inherit get-dest-directory

             render-flow
             render-part

             number-depth)

    (define footnote-elements '())

    (define/override (render-one part ri output-file)
      (set! footnote-ids (reverse footnote-ids))
      (set! footnote-elements '())
      (parameterize ([current-output-file output-file])
        (call-with-current-pygments-server
         (λ ()
           (define props (style-properties (part-style part)))
           (define post
             (rendered-post (content->string (strip-aux (part-title-content part)) this part ri)
                            (render-content (part-title-content part) part ri)
                            (or (and~> (findf document-date? props) document-date-text) "????-??-??")
                            (or (and~> (findf post-tags? props) post-tags-tags) '())
                            (append (render-flow (part-blocks part) part ri #f)
                                    (append-map (λ~> (render-part ri)) (part-parts part))
                                    (list `(div ([class "footnotes"])
                                             (ol ,@(for/list ([footnote-element (in-list (reverse footnote-elements))]
                                                              [footnote-index (in-naturals 1)])
                                                     `(li ([id ,(~a "footnote-" footnote-index)]) ,@footnote-element))))))))
           (write (serialize post))
           post))))

    (define/private (render-header part ri)
      (define props (style-properties (part-style part)))
      (define date-str (or (and~> (findf document-date? props) document-date-text) "????-??-??"))
      (define tag-strs (or (and~> (findf post-tags? props) post-tags-tags) '()))
      `(header
         (h1 ([class "title"]) ,@(render-content (part-title-content part) part ri))
         (div ([class "date-and-tags"])
           (time ([datetime ,date-str]) ,date-str)
           " ⦿ "
           ,@(render-content (add-between (map blog-tag tag-strs) ", ") part ri))))

    (define/override (render-part-content part ri)
      (define number (collected-info-number (part-collected-info part ri)))
      (define title
        (when/list (not (part-style? part 'hidden))
          `(,(header-depth->html-tag (number-depth number))
            ,@(cond/list
                [(part-title-content part)
                 => (λ (title-content) (render-content title-content part ri))]))))
      `[,@title
        ,@(render-flow (part-blocks part) part ri #f)
        ,@(append-map (λ~> (render-part ri)) (part-parts part))])

    (define/override (render-nested-flow i part ri starting-item?)
      (define rendered (append* (super render-nested-flow i part ri starting-item?)))
      (cond
        [(findf footnote-definition? (style-properties (nested-flow-style i)))
         => (λ (defn)
              (set! footnote-elements (cons rendered footnote-elements))
              '[])]
        [else
         `[(blockquote ,@rendered)]]))

    (define/override (render-paragraph e part ri)
      (match e
        [(paragraph (style #f (list (pygments-content source language))) '())
         `[(pre ,(pygmentize source #:language language))]]
        [_
         (define-values [tag-name attrs] (style->tag-name+attributes (paragraph-style e)))
         `[(,(or tag-name 'p)
            ,(attributes->list attrs)
            ,@(super render-paragraph e part ri))]]))

    (define/override (render-itemization e part ri)
      (define style (normalize-style (itemization-style e)))
      (define-values [tag-name attrs] (style->tag-name+attributes style))
      `[(,(or tag-name
              (if (eq? (style-name style) 'ordered) 'ol 'ul))
         ,(attributes->list attrs)
         ,@(for/list ([blocks (in-list (itemization-blockss e))])
             `(li ,@(render-flow blocks part ri #t))))])

    (define/public (render-title-link tag part ri)
      (define title-content
        (match (resolve-get part ri tag)
          [(vector title _ ...) title]
          [(blog-post title _) title]
          [(blog-post-anchor title _ _) title]))
      (render-content title-content part ri))

    (define/override (render-content elem part ri)
      (match elem
        [(? string? s) (list s)]
        [(element (style #f (list (pygments-content source language))) '())
         (list (pygmentize source #:language language))]
        [(element (style #f (list (footnote-reference note-id))) '())
         (define note-index (and~> (index-of footnote-ids note-id) add1))
         `[(sup (a ([href ,(if note-index
                               (~a "#footnote-" note-index)
                               "#")])
                   ,(or (~a note-index) "???")))]]
        [_
         (define style (normalize-style (if (element? elem) (element-style elem) #f)))
         (define-values [tag-name attrs] (style->tag-name+attributes style))

         (define (wrap-for-style rendered #:attrs [attrs attrs])
           ; First, we determine what element wrappers are needed by the style.

           ; alt-tag needs a custom wrapper
           (define alt-tag-wrap (if tag-name (cons tag-name (hasheq)) #f))

           ; target-url needs an 'a wrapper
           (define link-wrap
             (match (style-properties style)
               [(list _ ... (target-url target) _ ...)
                (cons 'a (hasheq 'href target))]
               [_ #f]))

           ; certain symbolic styles need wrappers
           (define style-name-wrap
             (match (style-name style)
               ['bold              (cons 'strong (hasheq))]
               [(or 'emph 'italic) (cons 'em (hasheq))]
               ['tt                (cons 'code (hasheq))]
               ['superscript       (cons 'sup (hasheq))]
               ['subscript         (cons 'sub (hasheq))]
               [_                  #f]))

           ; Now we combine the wrappers and mix in extra attributes.
           (define all-wraps
             (match (filter values (list alt-tag-wrap link-wrap style-name-wrap))
               ; If there are no wrappers, but we need to add attributes, add a
               ; 'span wrapper to hold them.
               ['()
                (if (hash-empty? attrs)
                    '()
                    (cons 'span attrs))]
               ; Otherwise, add the attributes to the outermost wrapper.
               [(cons (cons outer-tag outer-attrs) wraps)
                (cons (cons outer-tag (attributes-union outer-attrs attrs)) wraps)]))

           ; Finally, we apply all the wrappers.
           (for/fold ([rendered rendered])
                     ([wrap (in-list all-wraps)])
             `[(,(car wrap) ,(attributes->list (cdr wrap)) ,@rendered)]))

         (define rendered (if (and (link-element? elem)
                                   (null? (element-content elem)))
                              (render-title-link (link-element-tag elem) part ri)
                              (super render-content elem part ri)))
         (match elem
           [(target-element _ _ tag)
            (define anchor-name (tag->anchor-name (resolve-tag tag ri)))
            (define attrs* (attributes-union (hasheq 'name anchor-name) attrs))
            (wrap-for-style #:attrs (hasheq) `[(a ,(attributes->list attrs*) ,@rendered)])]

           [(link-element _ _ tag)
            (define dest (resolve-get part ri tag))
            (unless dest
              (error 'render "unknown link destination\n  tag: ~e" tag))

            (define href
              (match dest
                ; racket doc reference
                [(? vector?)
                 (define redirect-query
                   (cons (cons 'tag (tag->local-redirect-query-string tag))
                         (url-query external-tag-path)))
                 (url->string (struct-copy url external-tag-path [query redirect-query]))]

                [(blog-post _ path)
                 (relative-path->relative-url-string
                  (find-relative-path (get-dest-directory) path))]

                [(blog-post-anchor _ path anchor)
                 (define file-path path)
                 (define fragment (string-append "#" (uri-encode anchor)))
                 (if (equal? file-path (current-output-file))
                     fragment
                     (string-append (relative-path->relative-url-string
                                     (find-relative-path (get-dest-directory) file-path))
                                    fragment))]))

            (define attrs* (attributes-union (hasheq 'href href) attrs))
            (wrap-for-style #:attrs (hasheq) `[(a ,(attributes->list attrs*) ,@rendered)])]

           [_ (wrap-for-style rendered)])]))

    (super-new)))

(define (render-post-page info out)
  (match-define (rendered-post title-str title date tags body) info)
  (display "<!doctype html>" out)
  (xml:write-xexpr
   (page #:title title-str
         #:body `(div ([class "content"])
                   (article ([class "main"])
                     ,(build-post-header title date tags)
                     ,@body)))
   out))

(define (build-post-header title date tags)
  `(header
     (h1 ([class "title"]) ,@title)
     (div ([class "date-and-tags"])
       (time ([datetime ,date]) ,date)
       " " (span ([style "margin: 0 5px"]) "⦿") " "
       ,@(~> (for/list ([tag (in-list tags)])
               `(a ([href ,(tag-index-path tag)]) ,tag))
             (add-between ", ")))))

(define (render-post-index paths+infos out)
  (display "<!doctype html>" out)
  (xml:write-xexpr
   (page #:title "Alexis King’s Blog"
         #:body `(div ([class "content"])
                   ,@(for/list ([path+info (in-list paths+infos)])
                       (match-define (list path info) path+info)
                       (build-post-index-entry path info))))
   out))

(define (build-post-index-entry path info)
  (match-define (rendered-post _ title date tags body) info)
  `(article ([class "inline"])
     ,(build-post-header `[(a ([href ,path]) ,@title)] date tags)
     ; only render up to the start of the first section
     ,@(takef body (match-lambda [(cons 'h2 _) #f] [_ #t]))
     (p (a ([href ,path]) (span ([class "read-more-text"]) "Read more") " →"))))
