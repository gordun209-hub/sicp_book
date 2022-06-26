#!r6rs
(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs io simple (6)))
(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))
circumference
(define x 3)
x

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4)
;; application order
;; (sum-of-squares (+ 5 1) (* 5 2))
;; (+ (square (+ 5 1)) (square (* 5 2 )) )
;; (+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
;; followed by the reductions
;; (+ (* 6 6) (* 10 10))
;; (+ 36 100)
;; 136

(define (abs x)
  (cond
    [(> x 0) x]
    [(= x 0) 0]
    [(< x 0) (- x)]))
(abs 0) ;; 0

(define (better_abs x)
  (cond
    [(< x 0) (- x)]
    [else x]))
(better_abs 12) ;; 12

(define (endgame_abs x)
  (if (< x 0) (- x) x))

(endgame_abs 22) ;; 22

(and (> 5) (< x 10))
(define (>= x y)
  (or (> x y) (= x y)))

;; 5 + 4 + (2 - (3 - (6 + 4 \ 5))) / 3 * (6 - 2) * (2 - 7)
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

(define (square_sum_of_largest_two x y z)
  (if (and (> x z) (> y z)) (+ (* x x) (* y y)) (+ (* z z) (* y y))))

(square_sum_of_largest_two 5 2 1)

(define (sum-square x y)
  (+ (square x) (square y)))

(define (fun x y z)
  (cond
    [(and (<= x y) (<= x z)) (sum-square y z)]
    [(and (<= y x) (<= y z)) (sum-square x z)]
    [else (sum-square x y)]))

(define (a_plus_abs_a a b)
  ((if (> b 0) + -) a b))

;; if b is strictly a positive number, the operator expression
;; (if (> b a) + - ) will evaluate + the result will be a + b
;; oither cases, result will be a - b
;; in other words, function compute a + | b |.

(define (p)
  (p))
(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))
;; (test 0 )
;; test
;; (sqrt 12)
; -- Linear recursive process of factorial
(define (factorial n)
  (if (= n 0) 1 (* n (factorial (- n 1)))))

;; (factorial 6)
;; (* 6 (factorial 5))
;; (* 6 (* 5 (factorial 4)))
;; (* 6 (* 5 (* 4 (factorial 3))))
;; (* 6 (* 5 (* 4 (* 3 (factorial 2)))))
;; (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))
;; (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
;; (* 6 (* 5 (* 4 (* 3 2))))
;; (* 6 (* 5 (* 4 6)))
;; (* 6 (* 5 24))
;; (* 6 120)
;; 720

; A linear iterative process for computing factorial

(define (factorial_iter n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count) product (fact-iter (* counter product) (+ counter 1) max-count)))

;; better way
(define (factorial_iter_better n)
  (define (iter product counter)
    (if (> counter n) product (iter (* counter product) (+ counter 1))))
  (iter 1 1))
;; (factorial 6)
;; (fact-iter 1 1 6)
;; (fact-iter 1 2 6)
;; (fact-iter 2 3 6)
;; (fact-iter 6 4 6)
;; (fact-iter 24 5 6)
;; (fact-iter 120 6 6)
;; (fact-iter 720 7 6)
;; 720
(atom? (quote ()))
