#lang racket

(define zero (lambda (f) (lambda (x) x)))


;; take n
;; take f
;; take x
;; apply n to f
;; then call it with x
;; then apply f to all
(define add-1
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))

((add-1 zero) 1)
