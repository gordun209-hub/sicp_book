#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))
(define (car x)
  (x (lambda (p q) p)))

(define (cdr x)
  (x (lambda (p q) p)))

(car (cons 2 3))
