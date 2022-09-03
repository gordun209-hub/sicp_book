#lang sicp

;; Allysa p hacker designing a system to help people solve engineer
;; ing problems
;; Provide manipulate inexact quantities (such as measured parameters of
;; physical devices

(define (rp r1 r2)
  (/ 1 (+ (/ 1 r1) (/ 1 r2))))

;; resistor labeled with 6.8 ohms with %10 tolerance is between
;; 6.8 - 0.68 = 6.12 and 6.8 + 0.68 = 7.8 ohms

;; if you have 6.8 with 10% in parallel with a 4.7 ohm 5% resistor, resistance
;; of the combination can range from about 2.58 ohms if both low or 2.97
;; alyssa's idea is to implemet "interval arithmetic" as a set of arithmetic
;; operations for combining intervals

(define (make-interval a b) (cons a b))

(define (upper-bound a) (max (car a) (cdr a)))
(define (lower-bound a) (min (cdr a) (cdr a)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

;; exc 2.9
;; widht of an interval is half of the difference between upper and lower

;; a = (2 3) b = ( 1 2) aH = 3 aL = 2 bH = 2 bL = 1
;; (define (width a b)
;;   (/ (- (upper-bound a) (lower-bound a))))
;; width = 1/2 * ((aH + bH) - (aL + bL))
;;       = 1/2 * ((aH - aL) + (bH - bL))
;;       = width of interval a + width of interval b)

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))



;; Percent is between 0 and 100.0
(define (make-interval-center-percent c pct)
  (let ((width (* c (/ pct 100.0))))
    (make-interval (- c width) (+ c width))))

(define (percent-tolerance i)
  (let ((center (/ (+ (upper-bound i) (lower-bound i)) 2.0))
        (width (/ (- (upper-bound i) (lower-bound i)) 2.0)))
    (* (/ width center) 100)))
