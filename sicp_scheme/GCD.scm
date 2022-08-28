#lang racket
(define (square n) (* n n))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Testing for primality
;; this section describes two methods for checking the primality of an
;; integer n, one with order of growth O(sqrt n) and probabilistic alcorithm
;; with order of growth O(log n)


;; (define (smallest-divisor n)
;;   (find-divisor n 2))
;;
;; (smallest-divisor 199)
;; (smallest-divisor 1999)
;; (smallest-divisor 19999)

;(find-divisor 16 3)
;n = 16 test-divisor =3





;; Lame's theorem: If euclids algorithm requires k steps to compute the
;; GCD of some pair, then the smaller number in the pair must be greater
;; than or equal to the kth fib number

;;let's say 8 step required to compute GCD, according to lame, smaller number must be
;; greater than or equal to the

;; Fermats littl theorem, if n is a prime number and a is any possitive integer
;; less than n, then a raised to the nth power is congruent to a modulo n
;; Algorithm: given a number n, pick a random number a < n and compute the
;; remainder of a^n modulo n. if result is not equal to a, then n is
;; certainly  not prime. If it is a, then the chanches are good that n is prime
;; TODO anla?

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp  1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;(fast-prime? 222133 10)


;basic operations
(define (even? n)
  (= (remainder n 2) 0))

;; ex 1.23


(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next n)
    (if (= n 2) 3 (+ n 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (runtime)
  (current-milliseconds))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))


(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))
(define (report-prime n elapsed-time)
  (display n)
  (display "***")
  (display elapsed-time)
  (newline))
;search counter
(define (search-for-primes n counter)
  (if (even? n)
      (s-f-p (+ n 1) counter)
      (s-f-p n counter)))
;it's important to pay attention to the fact that predicate of the first 'if' here calls (timed-prime-test n) which in case of #t  computes into two procedures - (report-prime n (elapsed-time)) and 'then' case of the first 'if'.
;; TODO anla>?? mkk
(define (s-f-p n counter)
  (if (> counter 0)
      (if (timed-prime-test n)
          (s-f-p (+ n 2) (- counter 1))
          (s-f-p (+ n 2) counter))
      "COMPUTATION COMPLETE"))
(search-for-primes 1000 3)



(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

; See comments in exercise 1.22
(newline)
(timed-prime-test 1000000007)
(timed-prime-test 1000000009)
(timed-prime-test 1000000021)
(timed-prime-test 10000000019)
(timed-prime-test 10000000033)
(timed-prime-test 10000000061)
(timed-prime-test 100000000003)
(timed-prime-test 100000000019)
(timed-prime-test 100000000057)
(timed-prime-test 1000000000039)
(timed-prime-test 1000000000061)
(timed-prime-test 1000000000063)
;smallest divisor computation
;primality check
;check primality of consecutive odd integers in some range
;time&primality test
;drRacket has no (runtime) variable; had to substitute it with (current-milliseconds) which is basically same
