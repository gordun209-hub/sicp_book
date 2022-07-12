#lang sicp

;; 1. Special Forms
;; (a) and (and arg1 arg2 ...)
;; Evaluates arguments from left to right, stopping at the first one that evaluates to false
;; and returning false. Should all the arguments evaluate trueishly, returns the value of
;; the last argument.
;; (b) or (or arg1 arg2 ...)
;; Evaluates arguments from left to right, stopping at the first one that evaluates to trueish
;; and returns that value. Should all the arguments evaluate to false, returns false.


; 4 => number

; (+ 1 1) => number

(lambda (x) (+ x 1)) ; procedure

(lambda (x) (= x 1)) ; procedure

(define square
  (lambda (x) (* x x)))

square ; =>  procedure square

(square 5) ; 25

(define a 
  (lambda (f) (+ (f 5) 1)))

a ; => procedure 

(a square) ; => 26

(define b 
  (lambda (x y)
    (+ (a x ) y)))
b ; => procedure 
(b square 4)

(define c 
  (lambda (x)
    (lambda (y)
      (+ x y))))

c ; procedure 
((c 5) 2) ; procedure
