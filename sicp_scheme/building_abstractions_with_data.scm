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

(print-rat one-half)
;; 1/2

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
;; 5/6

(print-rat (mul-rat one-half one-third))
;; 1/6

(print-rat (add-rat one-third one-third))
;; 6/9

