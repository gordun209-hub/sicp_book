#lang sicp

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result) (cdr rest))))
  (iter initial sequence))

(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else
         (op (car seq) (accumulate op init (cdr seq))))))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(accumulate / 1 (list 1 2 3))

(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

(fold-right + 1 (list 1 2 3))
(fold-left + 1 (list 1 2 3))
