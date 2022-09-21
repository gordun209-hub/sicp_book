#lang sicp

(define (equal? l1 l2)
  (cond ((or (null? l1) (null? l2)) #t)
        ((not (= (length l1) (length l2)) )#f)
        ((not (eq? (car l1) (car l2)))
         #f)
        (else
         (equal? (cdr l1) (cdr l2)))))



(equal? '(this is a list) '(this is a list))

(equal? '(this is a list) '(this (is a ) list))

