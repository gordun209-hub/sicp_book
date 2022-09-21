#lang sicp

;; Alyssa P. Hacker doesnt see why if needs to be provided as a
;; Special form.

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)
;; 5
(new-if (= 1 1) 0 5)
;; 0

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))
;; writes square-root with new if
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

;; (sqrt 4) => does not terminate because
;; that this is not special form, compiler tries to eval
;; (sqrt-iter (improve guess x) x)
;; and if doesnt end
