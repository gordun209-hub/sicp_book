#lang racket/base

(define (add-rat x y)
  (make-rat (+ (* (numer x ) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (make-rat-but-stronger n d)
  (let ((g (gcd n d))
        (denom-sign (if (> d 0) 1 (- 1))))
    (cons (* (/ n g) denom-sign)
          (* (/ d g) denom-sign))))

(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(print-rat (make-rat 2 4))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))


(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))


; --------------------- exc 2.2
;; Consider the problem of representing line
;; segments in a plane. Each segment is represented as a pair
;; of points: a starting point and an ending point. Define a
;; constructor make-segment and selectors start-segment and
;; end-segment that define the representation of segments in
;; terms of points. Furthermore, a point can be represented
;; as a pair of numbers: the x coordinate and the y coordi-
;; nate. Accordingly, specify a constructor make-point and
;; selectors x-point and y-point that define this representa-
;; tion. Finally, using your selectors and constructors, define a
;; procedure midpoint-segment that takes a line segment as
;; argument and returns its midpoint (the point whose coor-
;; dinates are the average of the coordinates of the endpoints).
;; To try your procedures, you’ll need a way to print points:
(define make-point cons)
(define (x-point x) (car x))
(define (y-point y) (cdr y))
(define (midpoint p) (make-point ((/ (x-point p) 2)) ((/ (y-point p) 2))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))
(print-point (make-point 2 3))


(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (print-segment s)
  (newline)
  (display "[")
  (print-point (start-segment s))
  (display ",")
  (print-point (end-segment s))
  (display "]"))

(define (midpoint-segment s)
  (make-point (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2)
              (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)))

; --- Test ---

(print-point (make-point 2 3))

(define p1 (make-point 2 3))
(define p2 (make-point 5 6))
(define s1 (make-segment p1 p2))
(print-segment s1)
(print-point (midpoint-segment s1))


;; Exercise 2.3: Implement a representation for rectangles in
;; a plane. (Hint: You may want to make use of Exercise 2.2.) In
;; terms of your constructors and selectors, create procedures
;; that compute the perimeter and the area of a given rectan-
;; gle. Now implement a different representation for rectan-
;; gles. Can you design your system with suitable abstraction
;; barriers, so that the same perimeter and area procedures
;; will work using either representation?

(define (rect vertical horizontal) (cons vertical horizontal))
(define (horizontal-line rect) (car rect))
(define (vertical-line rect) (cdr rect))
(define (print-rectangle r)
  (newline)
  (display "(")
  (display (horizontal-line r))
  (display ",")
  (display (vertical-line r))
  (display ")")
  (newline))
(define (rect-area rect) (* (horizontal-line rect) (vertical-line rect)))
(print-rectangle (rect  5 2))
(rect-area (rect 5 2))

(define (conss x y)
  (lambda (m) (m x y)))
(define (carz z)
  (z (lambda (p q) p)))


(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 2)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(length odds)
