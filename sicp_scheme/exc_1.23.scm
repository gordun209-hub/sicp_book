#lang sicp

(define (square x) (* x x))

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor )n )n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next test-divisor)
  (cond ((= test-divisor 2) (+ test-divisor 1))
        (else (+ 2 test-divisor))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;(prime? 311)

;; Exc 1.25
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))


;; Parse error: Spurious closing paren found
(define (carmichel-test n)
  (define (iter a n)
    (cond ((= a n) #t)
          ((expmod a n n ) a) (iter (+ a 1) n)
          (else #f)))
  (iter 1 n))

(carmichel-test 561)
(carmichel-test 1105)
(carmichel-test 1729)
(carmichel-test 2465)
(carmichel-test 2821)
(carmichel-test 6601)
;; non-carmichel numbers to test if it works
(carmichel-test 10)
(carmichel-test 155)
(carmichel-test 121)
