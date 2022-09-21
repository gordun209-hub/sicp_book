#lang sicp
(define square (lambda (x) (* x x)))
;; b^n = b * b^n-1
;; b^0 = 1
;; O(n) space and time
(define (expt b n)
  (if (= n 0)
      1
      (* (b (expt b (- n 1))))))


;; O(n) steps and O(1) space
(define (expt-iterative b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))


;;we can compute using three multiplications

;; b^2 = b * b
;; b^4 = b^2 * b^2
;; b^8 = b^4 * b^4

;; this method works fine for exponents that are powers of 2
;; we can take advantage of successive squaring in computing
;; exponentials in general if we use the rule

;; b^n = (b^n/2)^2 if n is even
;; b^n = b*b^n-1   if n is odd
;; formulu aynen koda dokuoz
;; O(log n) 
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ;; n even ise squaresini al ve 2 ye bol n i
        ((even? n) (square (fast-expt b (/ n 2))))
        ;; deilse b ile carp ve n i 1 eksilt
        (else (* b (fast-expt b  (- n 1))))))




