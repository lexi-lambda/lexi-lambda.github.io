#lang racket/base

(require syntax/parse/define)

(provide when/list unless/list cond/list)

(define-simple-macro (when/list condition:expr body ...+)
  (if condition (list (let () body ...)) '()))
(define-simple-macro (unless/list condition:expr body ...+)
  (if condition '() (list (let () body ...))))
(define-simple-macro (cond/list clause ...)
  (cond clause ... [else '()]))
