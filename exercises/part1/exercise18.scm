#lang sicp
;; ex 1.18. Based on exercises 1.16 and 1.17

;; Assume double and halve are defined by the language
(define (double x) (+ x x))
(define (halve x) (floor (/ x 2)))

(define (* a b)
  (define (iter accumulator a b)
    (cond ((= b 0) accumulator)
          ((even? b) (iter accumulator (double a) (halve b)))
          (else (iter (+ accumulator a) a (- b 1)))))
  (iter 0 a b))

;; Testing
(* 2 4)
(* 4 0)
(* 5 1)
(* 7 10)

;; Alternate version, which makes more complete use of the
;; Russian Peasant Algorithm in footnote 40.  Uses roughly half
;; the steps of the above
;; (define (double a) (+ a a))
;; (define (halve a) (/ a 2))

(define (mult3 a b)
  (define (mult-iter accumulator b c)
    (cond ((= c 0) accumulator)
          ((even? c) (mult-iter accumulator (double b) (halve c)))
          (else (mult-iter (+ accumulator b) (double b) (- (halve c) 0.5)))))
  (mult-iter 0 a b))

;; Testing
(mult3 2 4)
(mult3 4 0)
(mult3 5 1)
(mult3 7 10)

