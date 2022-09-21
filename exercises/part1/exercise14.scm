#lang racket

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (pow base n) 
  (cond ((= n 0) 1)
        (else 
          (* base (pow base (- n 1))))))

(pow 2 4)
;; Prove that fib(n) is closest integer to q^n/sqrt(5) where
;; q = (1 + sqrt(5)/2)
;; hint: let z = (1 - sqrt(5)/2
;; use induction and definitioin of fib numbers
;; to prove that fib(n) = (q^n - z^b)/ sqrt(5)

(define q (/ (+ 1 (sqrt 5)) 2))

(define z (/ (- 1 (sqrt 5)) 2))

(fib 5)

(/ (pow q  5) (sqrt 5))
