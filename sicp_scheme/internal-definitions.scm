#lang sicp

(define (f x)
  (define (even? n) (if (= n 0) true (odd? (- n 1))))
  (define (odd? n) (if (= n 0) false (even? (- n 1))))
  (even? x))

(f 5)

(define (evenn? n) (if (= n 0) true (oddd? (- n 1))))
(define (oddd? n) (if (= n 0) false (evenn? (- n 1))))
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

(factorial 10)
