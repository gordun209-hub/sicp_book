#lang sicp

(define (product term a next b)
  (if (< b a)
      1
      (* (term a) (product term (next a) next b))))

(product + 1 inc 5)

;;1 * 2 * 3 * 4 * 5

(define (pi n)
  (define (term x) (* x x))
  (define (next x) (+ x 2))
  ; since we are increasing the numbers by two on every iteration
  (define limit (* n 2))
  ; upper term: - 2 always goes first, start building product from 4
  ;             - as 2 numbers are skipped, the limit must respect that, too
  ;             - since we are squaring one time too often at the end,
  ;               we have to divide that back out of the result
  ; lower term: start with 3, which is 1 more than the upper term
  ;             -> so increase limit by 1
  ;
  (* 4 (/ (/ (* 2 (product term 4 next (+ limit 2)))
             (+ limit 2))
          (product term 3 next (+ limit 1)))))

(pi 199)
