#lang sicp

(define (expt-1 b n)
  (if (= n 0) 1 (* b (expt b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))
;; This version requires O(n) steps and O(1) space

;; We can make this faster

;;b * (b * (b * (b))))
;; rather that, we can use this

;;b^2 = b * b
;; b^4 = b^2 * b^2
;; b^8 = b^4 * b^4

;; b^n = (b^n/2)^2 if n is even
;; b^n = b * (b^n-1) if n is odd
(define square
  (lambda (n) (* n n)))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n ) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
;; (fast-expt 4 2)
;; (square (fast-expt 4 1)
;; (square (* 4 (fast-expt 4 (0))))
;; (square (* 4 (1)))

;; Exercise 1.16
(define (iter-fast-expt b n)
  (define (iter N B A)
    (cond ((= 0 N) A)
          ((even? N) (iter (/ N 2) (square B) A))
          (else (iter (- N 1) B (* B A)))))
  (iter n b 1))

;; Exercise 1.17

;; (define (* a b)
;;   (if (= b 0)
;;       0
;;       (+ a (* a (- b 1)))))
;;
;; (* 2 3)

(define (add x y) (+ x y))
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define multiply
  (lambda (x y)
    (cond ((and (even? x) (even? y)) (multiply (* x 2) (/ y 2)))
          ((= y 0) x)
          (else (multiply (add x x) (- y 1))))))

(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (* a (halve b))))
        (else (+ a (* a (- b 1))))))

;; (* 2 4)

;; Exercise 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))
;; recall the transformatin of the state variables a and b in the fib-0iter
;; a <- a+b b <- a call this T
(define (fib-iter a b p q count)
  (cond ((= count 0 )b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)); compute p'
                   (+ (* 2 p q) (square q)); compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


