#lang sicp
(define (square x) (* x x))
(define (compose f g)
  (lambda (x)
   (f (g x))))

((compose square inc) 6)
