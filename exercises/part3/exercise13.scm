#lang sicp

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x
            x))

;; make cycle sets cdr of the list to list itself so infinite list
(define z (make-cycle (list 'a 'b 'c)))

