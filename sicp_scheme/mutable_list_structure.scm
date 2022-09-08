#lang sicp

;; (define (cons x y)
;;   (let ((new (get-new-pair)))
;;     (set-car! new x)
;;     (set-cdr! new y)
;;     new))

;; Exercise 3.12

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define x (list 'a 'b))

(define y (list 'c 'd))
(define z (append x y))
z

(cdr x)
; (b)

(define w (append! x y))
(cdr x)

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(last-pair '(2 3 4))
(define zz (make-cycle (list 'a 'b 'c)))


(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(mystery '(2 3 4 5))
