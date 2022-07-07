#lang racket/base

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (-
             (* (numer x) (denom y))
             (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))

(define (div-rat x y)


  (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y)) (* (numer y) (denom x))))

(define (make-rat n d)
  (let ([g (gcd n d)]) (cons (/ n g) (/ d g))))

(define (make-rat-but-stronger n d)
  (let ([g (gcd n d)] [denom-sign (if (> d 0) 1 (- 1))])
    (cons (* (/ n g) denom-sign) (* (/ d g) denom-sign))))

(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))
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
(define (x-point x)
  (car x))
(define (y-point y)
  (cdr y))
(define (midpoint p)
  (make-point ((/ (x-point p) 2)) ((/ (y-point p) 2))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))
(print-point (make-point 2 3))

(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

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

(define (rect vertical horizontal)
  (cons vertical horizontal))
(define (horizontal-line rect)
  (car rect))
(define (vertical-line rect)
  (cdr rect))
(define (print-rectangle r)
  (newline)
  (display "(")
  (display (horizontal-line r))
  (display ",")
  (display (vertical-line r))
  (display ")")
  (newline))
(define (rect-area rect)
  (* (horizontal-line rect) (vertical-line rect)))
(print-rectangle (rect 5 2))
(rect-area (rect 5 2))

(define (conss x y)
  (lambda (m) (m x y)))
(define (carz z)
  (z (lambda (p q) p)))

(define (list-ref items n)
  (if (= n 0) (car items) (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 2)

(define (length items)
  (if (null? items) 0 (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(length odds)

(define (length-but-with-iteration items)
  (define (length-iter a count)
    (if (null? a) count (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1) list2 (cons (car list1) (append (cdr list1) list2))))

; define a procedure last-pair that returns the list that contains only the
; last element of a given (nonempty) list:
; example: (last-pair (list 23 72 149 34)) -> 34
(define (last-pair l)
  (if (= (length l) 2) (cdr l) (last-pair (cdr l))))

;; (define (last-pair items)
;;   (if (null? items)
;;       items
;;       (last-pair (cdr items))))
(last-pair (list 2 3 55))
(cdr (cdr '(la mq oc)))

; define a procedure reverse that takes a list
(define nil '())
(define (reverse items)
  (define (iter items result)
    (if (null? items) result (iter (cdr items) (cons (car items) result))))

  (iter items nil))

(define (map proc items)
  (if (null? items) nil (cons (proc (car items)) (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

;; (define (square-list items)
;;   (map (lambda (x) (* x x)) items))
;; (square-list (list 1 2 3 4))
;;

;; (define (square-list items)
;;   (if (null? items)
;;       nil
;;       (cons (* (car items) (car items)) (square-list (cdr items)))))
;; (define (square-list items)
;; (map ⟨??⟩ ⟨??⟩))
;; (square-list (list 1 2 3 4))
(define (square x)
  (* x x))
(define (square-list items)
  (define (iter things answer)
    (if (null? things) answer (iter (cdr things) (cons (square (car things)) answer))))
  (iter items nil))

(square-list (list 5 2 3 1))

(define x (cons (list 1 2) (list 3 4)))
(display x)
(caddr '(a b c))
(define (count-leaves x)
  (cond
    [(null? x) 0]
    [(not (pair? x)) 1]
    [else (+ (count-leaves (car x)) (count-leaves (cdr x)))]))





