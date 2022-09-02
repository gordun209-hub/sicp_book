#lang sicp
(define (square x) (* x x))

(define (divides? a b) (= (remainder b a ) 0))

(define (next test-divisor)
  (cond ((= test-divisor 2) 3)
        (else (+ test-divisor 2))))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (smallest-divisor n) (find-divisor n 2))

(smallest-divisor 28)

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         ;; remainder of exponentiation?
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         ;; same thats like fast-square?
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 4 20)

; Exercise 1.25
;; (define (expmod base exp m)
;;   (remainder (fast-exp base exp) m))
