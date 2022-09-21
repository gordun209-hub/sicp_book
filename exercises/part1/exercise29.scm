#lang sicp
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

;; Simp rule
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(define (cube x) (* x x x))

(integral cube 0 1 0.001)
(define (round-to-next-even x) 
  (+ x (remainder x 2))) 

(define (simpson f a b n) 
  (define fixed-n (round-to-next-even n)) 
  (define h (/ (- b a) fixed-n)) 
  (define (simpson-term k) 
    (define y (f (+ a (* k h)))) 
    (if (or (= k 0) (= k fixed-n)) 
        (* 1 y) 
        (if (even? k) 
            (* 2 y) 
            (* 4 y)))) 
  (* (/ h 3) (sum simpson-term 0 inc fixed-n))) 

;; function f between a and b is approximated as 

;; h/3 (y0 + 4y1 + 2y2 + 4y3 + 2y4 + ... + 2yn-2 + 4yn-1 + yn)

;; where h = (b - a)/n for some even integer n and 
;; yk = f(a + kh) ;; increasing n increases the accuracy of approximation 
;; Define a procedure that takes as arguments f, a, b, and n 
;; returns the value of the integral computed using simp rule 

;; (define (simp-integral f a b n)
;;   (let* ((h (/ (- b a)))
;;          (termnum n)
;;          (ytermnum (f (+ a (* n h)))))
;;     (sum f)))
