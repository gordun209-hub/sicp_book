#lang sicp

(define (cube x) (* x x x))

(define (sum-integers-e a b)
  (if (> a b)
      0
      (+ a (sum-integers-e (+ a 1) b))))

(define (sum-cubes-e a b)
  (if (> a b) 0 (+ (cube a) (sum-cubes-e (+ a 1) b))))

(define (pi-sum-e a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum-e (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
;; general form
;; (define (<name> a b)
;;   (if (> a b)
;;       0
;;       (+ (<term> a)
;;          (<name> (<next> a) b))))


(define (inc n) (+ n 1))
(define (identity x) x)

(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-integers a b)
  (sum identity a inc b))

;; Example
(sum-integers 1 10) ;; 55

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


(integral cube 0 1 .01)
