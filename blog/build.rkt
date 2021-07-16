#lang racket/base

(require racket/class
         racket/date
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/promise
         racket/runtime-path
         racket/sequence
         (only-in megaparsack parse-result!)
         (only-in megaparsack/text parse-string)
         (only-in scribble/base title)
         scribble/base-render
         (only-in scribble/core document-date style)
         scribble/xref
         setup/xref
         threading

         (only-in "markdown/parse.rkt" document/p)
         "markdown/to-scribble.rkt"
         (only-in "scribble/post-language.rkt" post-tags)
         "scribble/render.rkt")

(define-runtime-path posts-dir "posts")

(define current-build-directory (make-parameter (string->path "build")))
(define current-output-directory (make-parameter (string->path "output")))

(struct post-dep (src-path main-part-promise) #:transparent)

(define (post-dep-main-part dep)
  (force (post-dep-main-part-promise dep)))

(define (post-dep-info-path dep)
  (~> (file-name-from-path (post-dep-src-path dep))
      (path-replace-extension #".info")
      (build-path (current-build-directory) _)))

(define (post-dep-xref-path dep)
  (~> (file-name-from-path (post-dep-src-path dep))
      (path-replace-extension #".sxref")
      (build-path (current-build-directory) _)))

(define (post-dep-page-path dep)
  (match-define (regexp #px"^(\\d{4})-(\\d{2})-(\\d{2})-(.+)$" (list _ year month day slug))
    (path->string (path-replace-extension (file-name-from-path (post-dep-src-path dep)) #"")))
  (build-path (current-output-directory) "blog" year month day slug "index.html"))

(define (output-path->absolute-url path)
  (define rooted-path (build-path "/" (find-relative-path (current-output-directory) path)))
  (path->string (if (equal? (path->string (file-name-from-path rooted-path)) "index.html")
                    (path-only rooted-path)
                    rooted-path)))

(define (markdown-post #:title title-str #:date date #:tags tags)
  (define path (build-path posts-dir (~a date "-" (tag->anchor-name (list title-str)) ".md")))
  (define main-part-promise
    (delay (document->part (parse-result! (parse-string document/p (file->string path)))
                           (title #:style (style #f (list (document-date date)
                                                          (post-tags tags)))
                                  title-str))))
  (post-dep path main-part-promise))

(define all-post-deps
  (list (markdown-post #:title "Parse, don’t validate"
                       #:date "2019-11-05"
                       #:tags (list "functional programming" "haskell" "types"))
        (markdown-post #:title "No, dynamic type systems are not inherently more open"
                       #:date "2020-01-19"
                       #:tags (list "types" "haskell" "programming languages" "functional programming"))
        (markdown-post #:title "Types as axioms, or: playing god with static types"
                       #:date "2020-08-13"
                       #:tags (list "types" "functional programming" "haskell" "typescript"))
        (markdown-post #:title "Names are not type safety"
                       #:date "2020-11-01"
                       #:tags (list "haskell" "types" "functional programming"))
        (markdown-post #:title "An introduction to typeclass metaprogramming"
                       #:date "2021-03-25"
                       #:tags (list "haskell" "types" "functional programming"))))

(define (timestamp-string)
  (define (pad n) (~r n #:min-width 2 #:pad-string "0"))
  (define now (current-date))
  (~a "[" (pad (date-hour now))
      ":" (pad (date-minute now))
      ":" (pad (date-second now))
      "]"))

(define (build-post-body dep)
  (define renderer (new (render-mixin render%)
                        [dest-dir (current-build-directory)]))
  (define main-parts (list (post-dep-main-part dep)))
  (define out-paths (list (post-dep-info-path dep)))
  (eprintf "~a running <posts>/~a\n" (timestamp-string) (find-relative-path posts-dir (post-dep-src-path dep)))

  (define traverse-info (send renderer traverse main-parts out-paths))
  (define collect-info (send renderer collect main-parts out-paths traverse-info))
  (xref-transfer-info renderer collect-info (load-collections-xref))
  (define resolve-info (send renderer resolve main-parts out-paths collect-info))

  (match-define (list (? rendered-post? post-info))
    (send renderer render main-parts out-paths resolve-info))

  (define out-info (send renderer serialize-info resolve-info))
  (call-with-output-file* #:exists 'truncate/replace
                          (post-dep-xref-path dep)
                          (λ~>> (write out-info)))

  (define undefined-tags (send renderer get-undefined resolve-info))
  (unless (empty? undefined-tags)
    (eprintf "Warning: some cross references may be broken due to undefined tags:\n")
    (for ([tag (in-list undefined-tags)])
      (eprintf "  ~s\n" tag)))

  post-info)

(define (build-post-page dep info)
  (define out-path (post-dep-page-path dep))
  (eprintf "~a rendering <output>/~a\n" (timestamp-string) (find-relative-path (current-output-directory) out-path))
  (make-parent-directory* out-path)
  (call-with-output-file* #:exists 'truncate/replace
                          out-path
                          (λ~>> (render-post-page info))))

(define (build-post-index number deps+infos)
  (define out-path (build-path (current-output-directory)
                               (~a "index" (if (= number 1) "" (~a "-" number)) ".html")))
  (eprintf "~a rendering <output>/~a\n" (timestamp-string) (find-relative-path (current-output-directory) out-path))
  (define paths+infos (for/list ([dep+info (in-list deps+infos)])
                        (match-define (list dep info) dep+info)
                        (list (output-path->absolute-url (post-dep-page-path dep)) info)))
  (call-with-output-file* #:exists 'truncate/replace
                          out-path
                          (λ~>> (render-post-index paths+infos))))

(define (build-all)
  (make-directory* (current-build-directory))
  (make-directory* (current-output-directory))
  (define post-infos
    (for/list ([dep (in-list all-post-deps)])
      (define info (build-post-body dep))
      (build-post-page dep info)
      info))
  (for ([deps+infos (in-slice 10 (reverse (map list all-post-deps post-infos)))]
        [number (in-naturals 1)])
    (build-post-index number deps+infos)))

(parameterize ([current-directory "/home/alexis/code/blog2"])
  (build-all))
