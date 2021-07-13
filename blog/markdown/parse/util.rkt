#lang racket/base

(require data/applicative
         (only-in data/functor [map map/p])
         data/monad
         megaparsack
         megaparsack/text)

(provide map/p
         newline/p
         many-till/p
         many+-till/p
         around/p
         around-string/p
         rest-of-line/p)

(define newline/p (label/p "newline" (or/p (string/p "\r\n") (string/p "\n"))))

(define (many-till/p p final-p)
  (define loop
    (or/p (do final-p (pure '()))
          (do [v <- p]
              [vs <- loop]
              (pure (cons v vs)))))
  loop)

(define (many+-till/p p final-p)
  (do [v <- p]
      [vs <- (many-till/p p final-p)]
      (pure (cons v vs))))

(define (around/p start-p p [end-p start-p])
  (do start-p (many-till/p p end-p)))
(define (around-string/p start-p [end-p start-p])
  (map/p list->string (around/p start-p any-char/p end-p)))

(define rest-of-line/p (map/p list->string (many-till/p any-char/p newline/p)))
