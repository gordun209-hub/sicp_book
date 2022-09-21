#lang sicp

;; recursive
(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))

;; iterative
(define (++ a b)
  (if (= a 0) b (++ (dec a) (inc b))))
