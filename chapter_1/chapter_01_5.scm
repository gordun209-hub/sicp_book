#lang sicp
; ExponentiationI
; O(n) O(n) space and time
;; (define (expt b n)
;;   (if (= n 0)
;;       1
;;       (* b (expt b (- n 1)))))
;;

; O(n)  time O(1) space
(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product ))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n ) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))
(define (square x) (* x x))

(fast-expt 4 4)

(define (fast-expt-iter a b n)
  (cond ((= n 0)
         a)
        ((even? n)
         (fast-expt-iter a (* b b) (/ n 2)))
        (else
         (fast-expt-iter (* a b) b (- n 1)))))

(fast-expt-iter 1 9 7)

; GCD(206, 40) = GCD(40,6)
;              = GCD(6,4)
;              = GCD(4,2)
;              = GCD(2,0)
;              = 2
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n  test-divisor)
  (cond ((> (square test-divisor) n )n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a )0))

(define (prime? n)
  (= n (smallest-divisor n)))
(prime? 33)

;fermats test

(define (expmod base exp m)
  (cond ((= exp 0 )1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m ))))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


