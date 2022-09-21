#lang sicp

(define (accumulate combiner null-value term a next b)
  (if (< b a)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a)  next b))))

(accumulate + 0 +  1 inc 10)

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(sum + 1 inc 10)

(define (iter-accum combiner null-value term a next b)
  (define (iter a res)
    (if (< b a)
      res
      (iter (next a) (combiner (term a) res))))
  (iter a null-value))

(iter-accum + 0 + 1 inc 10)
