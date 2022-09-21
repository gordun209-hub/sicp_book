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

(define (reverse sequence)
  (fold-right (lambda (x y) (cons x y)) nil sequence))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse '(1 2 3 4))
(reverse2 '(1 2 3 4))
;; (define (reverse sequence)
;;   (fold-right (lambda (x y) (cons y x) nil sequence)))
