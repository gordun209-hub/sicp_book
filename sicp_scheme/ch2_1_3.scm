#lang sicp
;; What is meant by data???


;; (define (cons x y)
;;   (define (dispatch m)
;;     (cond ((= m 0) x)
;;           ((= m 1) y)
;;           (else (error "err "))))
;;   dispatch)
;;
;; (define (car z) (z 0))
;; (define (cdr z) (z 1))



;; Exc 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons 1 2))
(car (cons 1 2))


(define (exp base n)
  (define (iter x result)
    ;; invariant: base^x * result is constant.
    (if (= 0 x)
        result
        (iter (- x 1) (* base result))))
  (iter n 1))


(define (count-0-remainder-divisions n divisor)
  (define (iter try-exp)
    (if (= 0 (remainder n (exp divisor try-exp)))
        (iter (+ try-exp 1))  ;; Try another division.
        (- try-exp 1)))

  ;; We don't need to try 0 divisions, as that will obviously pass.
  (iter 1))


;; cons, car, cdr
(define (my-cons a b) (* (exp 2 a) (exp 3 b)))
(define (my-car z) (count-0-remainder-divisions z 2))
(define (my-cdr z) (count-0-remainder-divisions z 3))


;; Usage:
(define test (my-cons 11 17))
(my-car test)
(my-cdr test)


;; Another way to define count-0-remainder-divisions

(define (divides? a b)
  (= 0 (remainder b a)))

;; (define (count-0-remainder-divisions n divisor)
;;   (define (iter x divisions)
;;     (if (divides? divisor x)
;;         (iter (/ x divisor) (+ divisions 1))
;;         divisions))
;;   (iter n 0))


;; Exc 2.6

(define zero (lambda () (lambda (x) x)))
;; Anlamadim TODO church numerals TODO
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)

;; (lambda (x) x)
;; (lambda (x) (f ((n f) x)))
;; n = (lambda (x) x)
;; (lambda (x)
