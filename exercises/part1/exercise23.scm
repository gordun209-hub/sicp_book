#lang sicp

;; after it checks to see if the number is divisible by 2
;; there is no point in checking to see if it is divisible by any
;; integer larger than even numbers
;; values used for test-divisor shoulkd not be
;; 2, 3, 4, 5, 6 ..., but rather 2, 3, 5, 7, 9

;; defune procedure next that returns 3 if its input is equal to 2 
;; othervise returns input plus 2 
(define (next n)
  (cond ((= n 2) 3)
        (else 
          (+ n 2))))

(define (square x) (* x x))
(define (divides? a b) (= (remainder b a) 0))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (prime? n)
  (= n (smallest-divisor n)))

;; O(sqrt n)
(find-divisor 31 5)
