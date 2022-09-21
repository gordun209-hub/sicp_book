#lang sicp
;; Define a procedure that takes three numbers as arguments and returns
;; the sum of squares of two larger ones

(define (sum-of-larger-two a b c)
  (cond ((and (> a b) (> c b)) (+ a c))
        ((and (> b a) (> c a)) (+ b c))
        (else (+ b a))))

(sum-of-larger-two 12 22 9)
