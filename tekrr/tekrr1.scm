#lang sicp

;Helpers

(define square
  (lambda (x)
    (* x x)))
; with define-syntax

(define (fdefine x y)
  (define (f-helper a b)
    (+ (* x (square a ))
       (* y b)
       (* a b)))
  (f-helper (+ (* x y))
            (- 1 y)))

; with lambda

(define (flambda x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

; with let (best)

(define (flet x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(define double
  (lambda (f)
    (lambda (x)
      (f (f x)))))


(define inc
  (lambda (x)
    (+ x 1)))

((double inc) 5)
(((double (double double)) inc) 5)

(define compose
  (lambda (p1 p2)
    (lambda (x)
      (p1 (p2 x)))))

((compose square inc) 6)
; ben ytaptm bunu fena mk 
(define repeated
  (lambda (p repat-time)
    (lambda (n)
      (cond
        ((= repat-time 0) n)
        (else
         (p ((repeated p (- repat-time 1))  n)))))))

((repeated square 2) 5)

; bunemk
(define (repeatedd f n)
  (define (iter t rf)
    (if (= t n) rf
        (iter (+ t 1) (compose f rf))))
  (iter 0 (lambda (x) x)))

; maeks sense
(define (repeateds f n)
  (if (= n 1)
      f
      (compose f (repeateds f (- n 1)))))
