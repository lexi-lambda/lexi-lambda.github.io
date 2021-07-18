#lang racket/base

(require racket/class
         racket/date
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/promise
         racket/sequence
         racket/serialize
         scribble/core
         scribble/xref
         setup/xref
         threading
         (only-in xml write-xexpr)

         "build/metadata.rkt"
         "build/render/scribble.rkt"
         "build/render/feed.rkt"
         "build/render/page.rkt"
         "lang/metadata.rkt"
         "markdown/post.rkt"
         "paths.rkt")

(define num-posts-per-page 10)

(struct post-dep (src-path main-part-promise) #:transparent)

(define (post-dep-main-part dep)
  (force (post-dep-main-part-promise dep)))

(define (post-dep-info-path dep)
  (~> (file-name-from-path (post-dep-src-path dep))
      (path-replace-extension #".info")
      (build-path build-dir _)))

(define (post-dep-xref-path dep)
  (~> (file-name-from-path (post-dep-src-path dep))
      (path-replace-extension #".sxref")
      (build-path build-dir _)))

;; Functions like `other-doc` assume that each top-level document has the tag
;; '(part "top") on its top-level section. This is only ensured by
;; setup/scribble, which we are not using, so we have to add it manually.
(define (ensure-top-tag main-part)
  (if (member '(part "top") (part-tags main-part))
      main-part
      (struct-copy part main-part
                   [tags (cons '(part "top") (part-tags main-part))])))

(define (set-blog-tag-prefix main-part path-str)
  (struct-copy part main-part [tag-prefix (blog-post-path->tag-prefix path-str)]))

(define (markdown-post file-name)
  (define path (build-path posts-dir file-name))
  (define main-part-promise (delay (~> (parse-markdown-post (file->string path) path)
                                       ensure-top-tag
                                       (set-blog-tag-prefix file-name))))
  (post-dep path main-part-promise))

(define all-post-deps
  (map markdown-post '("2015-07-18-automatically-deploying-a-frog-powered-blog-to-github-pages.md"
                       "2015-08-22-deploying-racket-applications-on-heroku.md"
                       "2015-08-30-managing-application-configuration-with-envy.md"
                       "2015-09-23-canonical-factories-for-testing-with-factory-girl-api.md"
                       "2015-11-06-functionally-updating-record-types-in-elm.md"
                       "2015-12-21-adts-in-typed-racket-with-macros.md"
                       "2016-02-18-simple-safe-multimethods-in-racket.md"
                       "2016-06-03-four-months-with-haskell.md"
                       "2016-08-11-climbing-the-infinite-ladder-of-abstraction.md"
                       "2016-08-24-understanding-the-npm-dependency-model.md"
                       "2016-10-01-using-types-to-unit-test-in-haskell.md"
                       "2017-01-02-rascal-a-haskell-with-more-parentheses.md"
                       "2017-01-05-rascal-is-now-hackett-plus-some-answers-to-questions.md"
                       "2017-04-28-lifts-for-free-making-mtl-typeclasses-derivable.md"
                       "2017-05-27-realizing-hackett-a-metaprogrammable-haskell.md"
                       "2017-06-29-unit-testing-effectful-haskell-with-monad-mock.md"
                       "2017-08-12-user-programmable-infix-operators-in-racket.md"
                       "2017-08-28-hackett-progress-report-documentation-quality-of-life-and-snake.md"
                       "2017-10-27-a-space-of-their-own-adding-a-type-namespace-to-hackett.md"
                       "2018-02-10-an-opinionated-guide-to-haskell-in-2018.md"
                       "2018-04-15-reimplementing-hackett-s-type-language-expanding-to-custom-core-forms-in-racket.md"
                       "2018-09-13-custom-core-forms-in-racket-part-ii-generalizing-to-arbitrary-expressions-and-internal-definitions.md"
                       "2018-10-06-macroexpand-anywhere-with-local-apply-transformer.md"
                       "2019-04-21-defeating-racket-s-separate-compilation-guarantee.md"
                       "2019-09-07-demystifying-monadbasecontrol.md"
                       "2019-10-19-empathy-and-subjective-experience-in-programming-languages.md"
                       "2019-11-05-parse-don-t-validate.md"
                       "2020-01-19-no-dynamic-type-systems-are-not-inherently-more-open.md"
                       "2020-08-13-types-as-axioms-or-playing-god-with-static-types.md"
                       "2020-11-01-names-are-not-type-safety.md"
                       "2021-03-25-an-introduction-to-typeclass-metaprogramming.md")))

(define (timestamp-string)
  (define (pad n) (~r n #:min-width 2 #:pad-string "0"))
  (define now (current-date))
  (~a "[" (pad (date-hour now))
      ":" (pad (date-minute now))
      ":" (pad (date-second now))
      "]"))

(define (write-html xexpr out)
  (display "<!doctype html>" out)
  (write-xexpr xexpr out))

(define (write-xml xexpr out)
  (display "<?xml version=\"1.0\" encoding=\"utf-8\"?>" out)
  (write-xexpr xexpr out))

(define (render-scribble render% main-part out-path
                         #:dest-dir [dest-dir (path-only out-path)]
                         #:xrefs-in [xref-in-paths '()]
                         #:xref-out [xref-out-path #f])
  (define main-parts (list main-part))
  (define out-paths (list out-path))
  (define renderer (new render% [dest-dir dest-dir]))

  (define traverse-info (send renderer traverse main-parts out-paths))
  (define collect-info (send renderer collect main-parts out-paths traverse-info))

  (for ([path (in-list xref-in-paths)])
    (define xref-in (call-with-input-file* path read))
    (send renderer deserialize-info xref-in collect-info))
  (xref-transfer-info renderer collect-info (load-collections-xref))

  (define resolve-info (send renderer resolve main-parts out-paths collect-info))
  (match-define (list render-result)
    (send renderer render main-parts out-paths resolve-info))

  (when xref-out-path
    (define out-info (send renderer serialize-info resolve-info))
    (call-with-output-file* #:exists 'truncate/replace
                            xref-out-path
                            (λ~>> (write out-info))))

  (define undefined-tags (send renderer get-undefined resolve-info))
  (unless (empty? undefined-tags)
    (eprintf "Warning: some cross references may be broken due to undefined tags:\n")
    (for ([tag (in-list undefined-tags)])
      (eprintf "  ~s\n" tag)))

  render-result)

(define (build-post-body dep)
  (define src-mod-time (file-or-directory-modify-seconds (post-dep-src-path dep) #f (λ () #f)))
  (define info-mod-time (file-or-directory-modify-seconds (post-dep-info-path dep) #f (λ () #f)))
  (cond
    [(and src-mod-time info-mod-time (> info-mod-time src-mod-time))
     (deserialize (call-with-input-file* (post-dep-info-path dep) read))]
    [else
     (eprintf "~a running <posts>/~a\n" (timestamp-string) (find-relative-path posts-dir (post-dep-src-path dep)))
     (render-scribble blog-post-render%
                      (post-dep-main-part dep)
                      (post-dep-info-path dep)
                      #:xref-out (post-dep-xref-path dep))]))

(define (build-post-page dep info)
  (define site-path (rendered-post-path info #:file? #t))
  (define out-path (reroot-path site-path output-dir))
  (eprintf "~a rendering <output>~a\n" (timestamp-string) site-path)
  (make-parent-directory* out-path)
  (call-with-output-file* #:exists 'truncate/replace
                          out-path
                          (λ~>> (write-html (post-page info)))))

(define (build-index-page total-pages page-number posts #:tag [tag #f])
  (define site-path (index-path page-number #:tag tag #:file? #t))
  (define out-path (reroot-path site-path output-dir))
  (eprintf "~a rendering <output>~a\n" (timestamp-string) site-path)
  (make-parent-directory* out-path)
  (call-with-output-file* #:exists 'truncate/replace
                          out-path
                          (λ~>> (write-html (index-page total-pages page-number posts #:tag tag)))))

(define (build-feeds posts #:tag [tag #f])
  (build-feed 'atom posts #:tag tag)
  (build-feed 'rss posts #:tag tag))

(define (build-feed type posts #:tag tag)
  (define site-path (feed-path type #:tag tag))
  (define out-path (reroot-path site-path output-dir))
  (eprintf "~a rendering <output>~a\n" (timestamp-string) site-path)
  (make-parent-directory* out-path)
  (call-with-output-file* #:exists 'truncate/replace
                          out-path
                          (λ~>> (write-xml (feed type posts #:tag tag)))))

(define (build-about-me)
  (define site-path "/about.html")
  (eprintf "~a rendering <output>~a\n" (timestamp-string) site-path)
  (local-require (only-in "posts/about-me.scrbl" [doc main-part]))
  (render-scribble (blog-standalone-page-render% standalone-page)
                   main-part
                   (reroot-path site-path output-dir)
                   #:xrefs-in (map post-dep-xref-path all-post-deps)))

(define (build-sitemap posts)
  (define site-path "/sitemap.txt")
  (eprintf "~a rendering <output>~a\n" (timestamp-string) site-path)
  (call-with-output-file*
   #:exists 'truncate/replace
   (reroot-path site-path output-dir)
   (λ (out)
     (displayln (full-url "/about.html") out)
     (for ([post (in-list posts)])
       (displayln (full-url (rendered-post-path post)) out)))))

(define (build-all)
  (make-directory* build-dir)
  (make-directory* output-dir)
  (define all-posts
    (for/list ([dep (in-list all-post-deps)])
      (define info (build-post-body dep))
      (build-post-page dep info)
      info))

  (define total-pages (ceiling (/ (length all-post-deps) num-posts-per-page)))
  (for ([posts (in-slice num-posts-per-page (reverse all-posts))]
        [number (in-naturals 1)])
    (build-index-page total-pages number posts))
  (build-feeds (reverse all-posts))

  (define tagged-posts
    (for*/fold ([tagged-deps+infos (hash)])
               ([post (in-list all-posts)]
                [tag (in-list (rendered-post-tags post))])
      (hash-update tagged-deps+infos tag (λ~>> (cons post)) '())))

  (for ([(tag posts) (in-immutable-hash tagged-posts)])
    (define total-pages (ceiling (/ (length posts) num-posts-per-page)))
    (for ([posts (in-slice num-posts-per-page posts)]
          [page-number (in-naturals 1)])
      (build-index-page total-pages page-number posts #:tag tag))
    (build-feeds posts #:tag tag))

  (build-about-me)
  (build-sitemap all-posts))

(module+ main
  (build-all))
