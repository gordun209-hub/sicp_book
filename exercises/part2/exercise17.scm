#lang sicp

(define (last-pair lst)
  (if (null? (cdr lst)) (car lst)
    (last-pair (cdr lst))))

(last-pair (list 23 72 149 34))


