#lang sicp


(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else 
          (op (car seq) (accumulate op init (cdr seq))))))
;; TODO
(define (dot-product v w)
  (accumulate + 0 (map * v w)))



