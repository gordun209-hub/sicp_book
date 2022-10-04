#lang sicp

(define x (cons 1 2))
(define z (cons x x))
z
x
(cdr z)
(set-car! (cdr z) 17)
(car x)
z
