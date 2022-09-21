#lang sicp

(define (square-list lst)
  (cond ((null? lst) '())
        (else 
          (cons (* (car lst) (car lst))
                (square-list (cdr lst))))))

(square-list '(2 3 4 5))

(define (square-list-with-map items)
  (map (lambda (x) (* x x)) 
       items))

(square-list-with-map '(2 3 4 5))
