#lang sicp

(define tolerance 0.00001)
(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)
(define (cube x) (* x x x))
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
((deriv cube) 5)

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt2 x)
  (fixed-point-of-transform 
    (lambda (y) (/ x y)) average-damp 1.0))

(define (cubic a b c) 
  (lambda (x) 
    (+ (cube x) 
       (* a (square x)) 
       (* b x) 
       c))) 

(define (sqrt3 x)
  (fixed-point-of-transform 
    (lambda (y) (- (square y) x)) newton-transform 1.0))

(newtons-method (cubic 1 10 2) 1)
