#lang racket
;; TODO?
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (let ((intt 0))
    (accumulate (lambda (this-coeff higher-terms)
                  (display "this-coeff: ")
                  (display this-coeff)
                  (newline)
                  (display "higher-terms :")
                  (display higher-terms)
                  (newline)
                  (+  (* higher-terms x)  this-coeff))
                0 coefficient-sequence)))

;; 1 + 0
(horner-eval 2 (list 1 0 3))


