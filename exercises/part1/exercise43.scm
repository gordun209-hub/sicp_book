#lang sicp

(define (square x) (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))
(define (repeat f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeat f (- n 1)))))

((repeat square 2) 5)
