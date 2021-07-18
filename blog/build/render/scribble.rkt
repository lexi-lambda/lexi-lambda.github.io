#lang racket/base

(require (for-syntax racket/base)
         racket/class
         racket/contract
         racket/format
         racket/hash
         racket/list
         racket/match
         racket/path
         racket/serialize

         scribble/base-render
         scribble/core
         scribble/html-properties
         (prefix-in scribble: scribble/html-render)
         (only-in scribble/private/render-utils part-style?)

         net/uri-codec
         net/url
         setup/dirs
         syntax/parse/define
         threading
         (only-in xml write-xexpr xexpr/c)

         "../../lang/metadata.rkt"
         "../../lang/post-language.rkt"
         "../../paths.rkt"
         "../metadata.rkt"
         "highlight/pygments.rkt"
         "util.rkt")

(provide base-render%
         blog-post-render%
         (contract-out
          [blog-standalone-page-render%
           (-> (-> #:title string? #:body (listof xexpr/c) xexpr/c)
               (implementation?/c render<%>))])

         blog-render-mixin
         pygments-render-mixin)

;; -----------------------------------------------------------------------------

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

(define (style->tag-name+attributes s ri)
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
      [(link-target tag)
       (values tag-name (attributes-union attrs
                                          (hasheq 'id (tag->anchor-name (resolve-tag tag ri)))))]
      [_
       (values tag-name attrs)])))

(define (resolve-tag base-tag ri)
  (add-current-tag-prefix (tag-key base-tag ri)))

(define tag->local-redirect-query-string
  (let ()
    (define racket-renderer (new (scribble:render-mixin render%)
                                 [dest-dir (find-system-path 'temp-dir)]))
    (with-method ([tag->query-string {racket-renderer tag->query-string}])
      (λ (tag) (tag->query-string tag)))))

; values associated with tags during the collect pass
(serializable-struct blog-page (title path) #:transparent
  #:guard (struct-guard/c content? site-path?))
(serializable-struct blog-page-anchor (title path anchor) #:transparent
  #:guard (struct-guard/c (or/c content? #f) site-path? string?))

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

;; Assists with the “overment + augride” trick used to hijack method dispatch
;; without otherwise disrupting downstream subclasses, as discussed here:
;; https://groups.google.com/g/racket-users/c/gM6DB7hY8nU/m/KaRpol06AQAJ
(define-syntax-parser class/hijack
  [(_ supercls:expr
      #:hijack-methods [hijack-id:id ...]
      body-form ...)
   #:with [super-hijack-id ...] (generate-temporaries (attribute hijack-id))
   #`(let ()
       (define-local-member-name super-hijack-id ...)
       (class #,(syntax/loc this-syntax
                  (class supercls
                    (define/public-final (super-hijack-id . args)
                      (super hijack-id . args))
                    ...
                    body-form ...))
         (inherit super-hijack-id ...)
         (define/augride (hijack-id . args)
           (super-hijack-id . args))
         ...
         (super-new)))])

(define-simple-macro (inner/hijack method:id . args)
  (inner (error 'inner/hijack "the impossible happened (maybe ~a wasn’t hijacked?)" 'method)
         method . args))

;; Like `render%` from scribble/base-render, but with some methods cleaned up or
;; replaced for greater consistency. In particular:
;;
;;   * The `current-output-file` and `current-top-part` parameters are set
;;     during the collect and render passes.
;;
;;   * `render-nested-flow` calls `render-flow` instead of calling `render-block`
;;     directly. As a side-effect of this change, `render-nested-flow` returns a
;;     flat list (of rendered blocks) rather than a list of lists.
(define base-render%
  (class/hijack render%
    #:hijack-methods [render-one]
    (inherit collect-part render-flow)

    (define/override (start-collect ds fns ci)
      (for-each (lambda (d fn)
                  (parameterize ([current-output-file fn]
                                 [current-top-part d])
                    (collect-part d #f ci null 1 #hash())))
                ds
                fns))

    (define/overment (render-one part ri output-file)
      (parameterize ([current-output-file output-file]
                     [current-top-part part])
        (inner/hijack render-one part ri output-file)))

    (define/override (render-nested-flow i part ri starting-item?)
      (render-flow (nested-flow-blocks i) part ri #t))

    (super-new)))

;; The foundational renderer mixin used by this blog. It is an HTML renderer,
;; but it is drastically stripped down and streamlined relative to the built-in
;; renderer provided by scribble/html-render.
;;
;; This mixin does not, by itself, support all the functionality used by blog
;; posts themselves---for example, it does not support footnotes---but it is a
;; useful foundation that can be used for rendering non-post pages.
(define (blog-render-mixin %)
  (class %
    (define/override (current-render-mode) '(html))
    (define/override (get-suffix) #".html")

    (define external-tag-path (string->url (get-doc-search-url)))
    (define/override (set-external-tag-path p)
      (set! external-tag-path (string->url p)))

    ;; Returns the (absolute) path portion of the URL for the page that contains
    ;; the part currently being rendered.
    (define/public (get-current-site-path)
      (path->string (build-path "/" (find-relative-path (get-dest-directory)
                                                        (current-output-file)))))

    ;; -------------------------------------------------------------------------
    ;; collect

    (define/public (part-whole-page? p ri)
      (match (resolve-get p ri (car (part-tags p)))
        ; racket doc reference; index 4 corresponds to `whole-page?`
        [(? vector? dest) (vector-ref dest 4)]
        ; blog references
        [(? blog-page?) #t]
        [(? blog-page-anchor?) #f]))

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
        (define title (or (part-title-content d) "???"))
        (collect-put! ci key
                      (if (current-part-whole-page? d)
                          (blog-page title (get-current-site-path))
                          (blog-page-anchor title
                                            (get-current-site-path)
                                            (tag->anchor-name (add-current-tag-prefix key)))))))

    (define/public (collect-link-target tag ci)
      (define key (generate-tag tag ci))
      (collect-put! ci key (blog-page-anchor #f
                                             (get-current-site-path)
                                             (tag->anchor-name (add-current-tag-prefix key)))))

    (define/private (collect-link-target-from-style style ci)
      (cond
        [(findf link-target? (style-properties style))
         => (λ (target) (collect-link-target (link-target-tag target) ci))]))

    (define/override (collect-nested-flow i ci)
      (collect-link-target-from-style (nested-flow-style i) ci)
      (super collect-nested-flow i ci))

    (define/override (collect-paragraph i ci)
      (collect-link-target-from-style (paragraph-style i) ci)
      (super collect-paragraph i ci))

    (define/override (collect-content i ci)
      (when (and (element? i) (style? (element-style i)))
        (collect-link-target-from-style (element-style i) ci))
      (super collect-content i ci))

    (define/override (collect-target-element i ci)
      (when (redirect-target-element? i)
        (raise-arguments-error 'collect-target-element "redirect targets not supported"
                               "element" i))
      (collect-link-target (target-element-tag i) ci))

    (define/override (resolve-content i d ri)
      (cond
        [(and external-tag-path
              (link-element? i)
              (style? (element-style i))
              (memq 'indirect-link (style-properties (element-style i))))
         ; Don’t resolve indirect links, or else we’ll get spurious warnings
         ; about undefined tags.
         (resolve-content (element-content i) d ri)]
        [else
         (super resolve-content i d ri)]))

    ;; -------------------------------------------------------------------------
    ;; render

    (inherit get-dest-directory

             render-flow
             render-part

             number-depth)

    (define/override (render-one part ri output-file)
      (define xexpr
        `(html
           (head (title ,(content->string (strip-aux (part-title-content part)) this part ri)))
           (body ,@(render-part part ri))))
      (write-xexpr xexpr)
      xexpr)

    (define/override (render-part-content part ri)
      (define number (collected-info-number (part-collected-info part ri)))
      (define anchors (for/list ([tag (in-list (part-tags part))])
                        `(a ([name ,(tag->anchor-name
                                     (add-current-tag-prefix
                                      (tag-key tag ri)))]))))
      (define title
        (when/list (not (part-style? part 'hidden))
          `(,(header-depth->html-tag (number-depth number))
            ,@anchors
            ,@(cond/list
                [(part-title-content part)
                 => (λ (title-content) (render-content title-content part ri))]))))
      `[,@title
        ,@(render-flow (part-blocks part) part ri #f)
        ,@(append-map (λ~> (render-part ri)) (part-parts part))])

    (define/override (render-nested-flow i part ri starting-item?)
      (define-values [tag-name attrs] (style->tag-name+attributes (nested-flow-style i) ri))
      `[(,(or tag-name 'blockquote)
         ,(attributes->list attrs)
         ,@(super render-nested-flow i part ri starting-item?))])

    (define/override (render-paragraph e part ri)
      (define-values [tag-name attrs] (style->tag-name+attributes (paragraph-style e) ri))
      `[(,(or tag-name 'p)
         ,(attributes->list attrs)
         ,@(super render-paragraph e part ri))])

    (define/override (render-itemization e part ri)
      (define style (itemization-style e))
      (define-values [tag-name attrs] (style->tag-name+attributes style ri))
      `[(,(or tag-name
              (if (eq? (style-name style) 'ordered) 'ol 'ul))
         ,(attributes->list attrs)
         ,@(for/list ([blocks (in-list (itemization-blockss e))])
             `(li ,@(render-flow blocks part ri #t))))])

    (define/public (render-title-link tag part ri)
      (define title-content
        (match (resolve-get part ri tag)
          [(vector title _ ...) title]
          [(blog-page title _) title]
          [(blog-page-anchor title _ _) title]
          [#f "???"]))
      (render-content title-content part ri))

    (define/override (render-content elem part ri)
      (cond
        [(string? elem) (list elem)]
        [else
         (define style (normalize-element-style (if (element? elem) (element-style elem) #f)))
         (define-values [tag-name attrs] (style->tag-name+attributes style ri))

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
                    (list (cons 'span attrs)))]
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
            (define indirect? (memq 'indirect-link (style-properties style)))
            (define dest (and (not indirect?) (resolve-get part ri tag)))
            (define href
              (match dest
                ; racket doc reference
                [(or #f (? vector?))
                 (define redirect-query
                   (cons (cons 'tag (tag->local-redirect-query-string tag))
                         (url-query external-tag-path)))
                 (url->string (struct-copy url external-tag-path [query redirect-query]))]

                [(blog-page _ path)
                 (site-path->url-string path)]

                [(blog-page-anchor _ path anchor)
                 (if (equal? path (get-current-site-path))
                     (string-append "#" (uri-encode anchor))
                     (site-path->url-string path #:fragment anchor))]))

            (define attrs* (attributes-union (hasheq 'href href) attrs))
            (wrap-for-style #:attrs (hasheq) `[(a ,(attributes->list attrs*) ,@rendered)])]

           [_ (wrap-for-style rendered)])]))

    (super-new)))

;; Recognizes `paragraph`s and `element`s with a `pygments-content` style
;; property and replaces them with the result of running Pygments on the content
;; in the property. Body content and other style properties are ignored.
(define (pygments-render-mixin %)
  (class/hijack %
    #:hijack-methods [render-one]

    (define/overment (render-one part ri output-file)
      (call-with-current-pygments-server
       (λ () (inner/hijack render-one part ri output-file))))

    (define/override (render-paragraph e part ri)
      (match (findf pygments-content? (style-properties (paragraph-style e)))
        [(pygments-content source language)
         `[(pre ,(pygmentize source #:language language))]]
        [_ (super render-paragraph e part ri)]))

    (define/override (render-content e part ri)
      (match e
        [(element (style _ (app (λ~>> (findf pygments-content?))
                                (pygments-content source language)))
                  _)
         (list (pygmentize source #:language language))]
        [_ (super render-content e part ri)]))

    (super-new)))

(define (blog-standalone-page-render% page-template)
  (class (pygments-render-mixin (blog-render-mixin base-render%))
    (inherit render-part)

    (define/override (render-one part ri output-file)
      (define xexpr
        (page-template
         #:title (content->string (strip-aux (part-title-content part)) this part ri)
         #:body (render-part part ri)))
      (write-xexpr xexpr)
      xexpr)
    
    (super-new)))

;; The renderer used for actual blog posts, with all the bells and whistles.
(define blog-post-render%
  (class (pygments-render-mixin (blog-render-mixin base-render%))
    (inherit render-content
             render-flow
             render-part)

    ; We don’t render blog posts directly to HTML because we need to be able to
    ; render them in different ways on different pages, so we generate `.info`
    ; files containing serialized `rendered-post` structures, instead.
    (define/override (get-suffix) #".info")

    (define/override (get-current-site-path)
      (define part (current-top-part))
      (post-path (get-required-style-prop post-date? (style-properties (part-style part)))
                 (content->string (strip-aux (part-title-content part)))))

    (define/override (render-one part ri output-file)
      (define props (style-properties (part-style part)))
      (define title-str (content->string (strip-aux (part-title-content part)) this part ri))
      (define title-content (render-content (part-title-content part) part ri))
      (define body-content (append (render-flow (part-blocks part) part ri #t)
                                        (append-map (λ~> (render-part ri)) (part-parts part))))
      (define post
        (rendered-post title-str
                       title-content
                       (get-required-style-prop post-date? props)
                       (post-tags-tags (get-required-style-prop post-tags? props))
                       body-content))
      (write (serialize post))
      post)

    (define/private (get-required-style-prop pred? props)
      (or (findf pred? props)
          (raise-arguments-error 'render "missing required style property on main part"
                                 "output file" (current-output-file)
                                 "expected" (unquoted-printing-string (~a (contract-name pred?)))
                                 "properties" props)))

    (super-new)))
