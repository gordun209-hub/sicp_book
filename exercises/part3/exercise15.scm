#lang sicp

(define x (list 'a 'b))
(define z1 (cons x x))

(define (set-to-wow! x) (set-car! (car x) 'wow)x)
(define z2 (cons (list 'a 'b) (list 'a 'b)))


z1 

(set-to-wow! z1)

z2
(set-to-wow! z2)
