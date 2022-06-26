#lang scheme
(define (sqrt-iter guess x)
  (if (good-enough? guess x) guess (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond
    [predicate then-clause]
    [else else-clause]))
;; that new if causes infinite loop bec unlike 'if', 'new-if' is a function
;; and it evaluates its inside
;; (define (sqr-iter guess x)
;;   (new-if (good-enough? guess x) guess (sqr-iter (improve guess x) x)))
;; (sqr-iter 1 3)
