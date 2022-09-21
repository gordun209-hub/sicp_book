#lang sicp

;; Write new good-enoug? for working small and large numbers
;; we need to compare current guess with prev guess
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (= (improve guess x) guess)) (define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))


(sqrt 4000000)
