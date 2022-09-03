#lang sicp

(define make-ratt cons)
(define numer car)
(define denom cdr)


(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
;; exc 2.1
(define (better-make-rat n d)
  (cond ((and (< n 0) (< d 0)) (cons (- 0 (/ n (gcd n d))) (- 0 (/ d (gcd n d)))))
        ((< d 0) (cons (- 0 (/ n (gcd n d))) (- 0 (/ d (gcd n d)))))
        (else (cons n d))))

(better-make-rat 3 -2)

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; clewer
(define (make-rattt n d)
  (let ((g ((if (< d 0) - +) (gcd n d))))
    (cons (/ n g) (/ d g))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))


(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (* (numer x) (numer y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (* (numer x) (denom y))
               (* (denom x) (numer y)))))

(define one-half (make-rat 1 2))

;; (print-rat one-half)
;; 1/2

(define one-third (make-rat 1 3))
;; (print-rat (add-rat one-half one-third))
;; ;; 5/6
;;
;; (print-rat (mul-rat one-half one-third))
;; ;; 1/6
;;
;; (print-rat (add-rat one-third one-third))
;; 6/9


;; Underlying idea of data abstraction is to identify for each type
;; of data object a basic set of operations in terms of which all
;; manipulations of data objects of that type will be expressed, and then
;; only those operations in manipulating the data

(define (make-rat-alternate n d)
  (cond (n d)))

(define (numer-alternate x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom-alternate x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))


;; Exc 2.2
;; consider the problem of representing line segments in plane,
;; Each segment is represented as a pair of points
(define (make-segment a b) (cons a b))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))


;; (midpoint-segment segment1)
;; ;; clewr
;; (define (midpoint-segment segment)
;;   (define (average a b) (/ (+ a b) 2.0))
;;   (let ((a (start-segment segment))
;;         (b (end-segment segment)))
;;     (make-point (average (x-point a)
;;                          (x-point b))
;;                 (average (y-point a)
;;                          (y-point b)))))
;; (define (print-point p)
;;   (newline)
;;   (display "(")
;;   (display (x-point p))
;;   (display ",")
;;   (display (y-point p))
;;   (display ")"))

;; Exercise 2.3

(define (average-points a b)
  (/ (+ (x-point a) (x-point b))
     (+ (y-point a) (y-point b))
     2))

(define point1 (make-point 3 4))
(define point2 (make-point 1 3))
(define point3 (make-point 1 2))
(define point4 (make-point 1 1))
(define segment1 (make-segment point1 point2))
(define segment2 (make-segment point3 point4))
(define make-rectangle cons) 
(define rect (make-rectangle segment1 segment2))
rect

;; (define (area-rectangle rect))

