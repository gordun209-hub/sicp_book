#lang sicp
;; Helper procedures
(define (square x) (* x x))

(define (fib x)
  (if (<= x 2) 1 (+ (fib (- x 1)) (fib (- x 2)))))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;; (even-fibs 40)
; they both common at

; Enumerates the leaves of a tree
; Filters them, selecting the odd ones
; Squares each of the selected ones; and
; accumulates the resulkt using +, starting with 0


;; Sequence Operations
;; e key to organizing programs so as to more clearly reflect the signal-
;; flow structure is to concentrate on the “signals” that flow from one stage
;; in the process to the next. If we represent these signals as lists, then we
;; can use list operations to implement the processing at each of the stages.
;; For instance, we can implement the mapping stages of the signal-flow



;; (map square (list 1 2 3 4 5))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; (accumulate + 0 (list 1 2 3 4 5))


(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;; (length (enumerate-interval 2 1200000))

(define (enumerate-tree tree)
  (cond
    ((null? tree) nil)
    ((not (pair? tree)) (list tree))
    (else
     (append
      (enumerate-tree (car tree))
      (enumerate-tree (cdr tree))))))

;; (enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squaresg tree)
  (accumulate
   +
   0
   (map square (filter odd? (enumerate-tree tree)))))

;; (sum-odd-squaresg (list 2 (list 9 (list 27))))

(define (even-fibsg n )
  (accumulate
   cons
   nil
   (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate
   cons
   nil (map square (map fib (enumerate-interval 0 n)))))

;; (list-fib-squares 20)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate
   *
   1
   (map square
        (filter odd?
                sequence))))

;; (product-of-squares-of-odd-elements (list 1 2 3 4 5))

;; (define (salary-of-highest-paid-programmer records)
;;   accumulate
;;   max
;;   0
;;   (map salary
;;        (filter programmer?
;;                records)))
