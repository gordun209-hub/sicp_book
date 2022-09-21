#lang sicp

;; create function that

;; f(n) = n if n < 3,
;;      = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3

(define (f n)
  (cond ((< n 3) n)
        (else
         (+ (f(- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

;;
(define (fii n) (fi n 0 1 2))
(define (fi i a b c)
  (cond ((< i 0) i)
        ((= i 0) a)
        (else (fi (- i 1) b c (+ c (* 2 b) (* 3 a))))))

