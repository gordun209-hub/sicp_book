#lang sicp

;; Newtons method for cube root
;; x/y^2 + 2y / 3

(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x) x)))

(define (improve y x)
  (/ (+ (* 2 y) (/ x (* y y)))
     3))
(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(define (square x) (* x x))

(define (cube x)
  (cube-iter 1.0 x))

(cube 64)

