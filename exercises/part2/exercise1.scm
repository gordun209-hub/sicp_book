#lang sicp

;; (define (make-rat n d)
;;   (let ((g (gcd n d)))
;;     (cons (/ n g) (/ d g))))


(define (make-rat numer denom)
  (let ((g (gcd numer denom)))
    (cond ((and (< denom 0) (< numer 0))
           (cons (/ (- 0 numer) g) (/ (- 0 denom) g)))
          ((and (> denom 0) (< numer 0))
           (cons (/ numer g) (/ denom g)))
          (else
           (cons (/ (- 0 numer) g) (/ (- 0 denom) g))))))

(define numer car)
(define denom cdr)

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
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))


(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


;; (define one-half (make-rat 1 2))
;; (print-rat one-half)
;; ;; 1/2
;;
;; (define one-third (make-rat 1 3))
;; (print-rat (add-rat one-half one-third))
;; ;; 5/6
;;
;; (print-rat (add-rat one-third one-third))

;; Define better version of make-rat that handles both positive and neg

(print-rat (make-rat 1 -3))
(print-rat (make-rat -3 -1))
(print-rat (make-rat -3 1))

;; (define x (list (list 1 2) (list 3 4)))

;; (reverse x)
;; (define (deep-reverse lst)
;;   (cond ((null? lst) '())
;;         ((pair? lst) (reverse lst))
;;         (else
;;          (cons (deep-reverse (car lst))
;;                (deep-reverse (cdr lst))))))
;;
;; (deep-reverse x)

;; (define x (list (list 1 2) (list 3 4)))
;;
;; (define (fringe lst)
;;   (cond ((null? lst) '())
;;         ((not (pair? (car lst))) (cons (car lst) (fringe (cdr lst))))
;;         (else
;;          (append (fringe (car lst))
;;                (fringe (cdr lst))))))
;;
;;
;; (fringe (list x x))
