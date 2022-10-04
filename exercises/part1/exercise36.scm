#lang sicp


(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display "guess is =")
      (display guess)
      (newline)
      (display "next guess is =")
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (average x y) (/ (+ x y) 2))


(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)


