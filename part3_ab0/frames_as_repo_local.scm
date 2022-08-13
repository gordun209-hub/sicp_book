#lang sicp

(define (make-withdraw-2 balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw-2 100))

(W1 50) ; 50

(define W2 (make-withdraw-2 100))


(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
(define square
  (lambda (x) (* x x)))

(define average
  (lambda (x y) (/ 2 (+ x y))))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x )) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

