#lang sicp


;; suppose initial guess is 1

;; Guess     Quotient              Average
;; 1         (2/1) = 2          ((2 + 1) / 2) = 1.5
;; 1.5       (2/1.5)=1.333      ((1.4167 + 1.4118)/2) = 1.4167
;; 1.1467    (2/1.4167)=1.4118  ((1.4167 + 1.4118)/2) = 1.4142
;; 1.4142     ...               ...
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))


(sqrt 4000000000)
