#lang sicp
(define atom?
  (lambda (x)
    (and (not (pair? x))  (not (null? x)))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp))
            (numbered?
             (car (cdr (cdr aexp)))))))))

(numbered? '(2 + w))
(number? +)

(define 1st-sub-exp-bad
  (lambda (aexp)
    (cond
      (else (car (cdr aexp))))))

(define 1st-sub-exp 
  (lambda (aexp)
    (car (cdr aexp))))

(car (cdr '(2 + 3)))

(define 2st-sub-exp 
  (lambda (aexp)
    (car (cdr (cdr (aexp))))))

(define operator
  (lambda (aexp)
    (car aexp)))

; holy sht its happening!

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ())  n )))

(define zub1
  (lambda (n)
    (cdr n)))

(define plus
  (lambda (n m)
    (cond 
      ((sero? m ) n)
      (else (edd1 (plus (zub1 m)))))))







