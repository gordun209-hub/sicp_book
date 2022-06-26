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

(define (sqrt_1 x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x) guess (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(define (sqrt_2 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess) guess (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


