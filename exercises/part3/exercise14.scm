#lang sicp

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(mystery '(1 23 4 5))
;; reverses with temp
(define w (mystery '(1 23 4 5)))

w
