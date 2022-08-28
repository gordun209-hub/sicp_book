#lang racket/base

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0 )) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4 )25)
        ((= kinds-of-coins 5) 50)))

(count-change 100)

;; Exercise 1.11

;; f(n) = n if n < 3 and f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n>=3

(define (f n)
  (cond ((< n 3) n)
        ((>= n 3) (+
                   (f (- n 1))
                   (f (- n 2))
                   (* 3
                      (f (- n 3)))))))

;(f 5) ; => 17

;; Exercise 1.12 Pascal's triangle
;; Write a procedure that computes pascals triangle




(define (pascal r c)
  (if (or (= c 1) (= c r))
      1
      ;; p(3 2) = p(2 1) + p(2 2)
      (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))))

;; Exercise 1.13
;; teta = (1 + sqrt5)/2

;;Exercise 1.15
;; sinx = 3 * sin x/3 - 4 * sin^3 x/3

;; to reduce the size of the argument of sin
;;its magnitude is not greater that 0.1 radians
(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))



