#lang sicp

(define (for-each proc lst)
  (cond ((null? lst) 'done)
        (else 
          (for-each proc (cdr lst))
          (proc (car lst)))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 565 2 1321 1))
