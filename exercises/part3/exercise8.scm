#lang sicp


;; return 0 if left to right
;; define 1 right to left
(define (g y)
  (define (f x)
    (let ((z y))
      (set! y x)
      z))
  f)
(define f (g 0))




(+ (f 0) (f 1))
