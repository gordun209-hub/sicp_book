#lang sicp

(define (square x) (* x x))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) (* guess .001)))


(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 16) ; => 4.002025...
(sqrt 25); => 5.000023... 
(sqrt 0.9); => 0.9486...



