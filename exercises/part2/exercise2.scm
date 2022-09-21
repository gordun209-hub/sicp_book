#lang sicp

;; Consider the problem of representing line segments in a plane
;; Each segment is represented as a pair so =>
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)


(define make-point cons)
(define x-point car)
(define y-point cdr)
(define (average x y) (/ (+ x y) 2))

(define (midpoint seg)
  (make-segment (average (start-segment (x-point seg))
                         (start-segment (y-point seg)))

                (average (end-segment (x-point seg))
                         (end-segment (y-point seg)))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define mypoint (make-segment 3 4))
(define mypoint1 (make-segment 1 2))
(define myseg (make-segment mypoint1 mypoint))
(midpoint myseg)
(print-point mypoint)
