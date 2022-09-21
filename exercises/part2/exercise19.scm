#lang sicp

;; Helper methods, could define them in cc too:
(define (first-denomination denominations) (car denominations))
(define (except-first-denom denominations) (cdr denominations))
(define (no-more? denominations) (null? denominations))

(define (cc amount denominations)
  (cond
   ;; If there's no change left, we have a solution
   ((= amount 0) 1)

   ;; If we're gone -ve amount, or there are no more kinds of coins
   ;; to play with, we don't have a solution.
   ((or (< amount 0) (no-more? denominations)) 0)

   (else
    ;; number of ways to make change without the current coin type
    ;; plus the number of ways after subtracting the amount of the
    ;; current coin.
    (+ (cc amount (except-first-denom denominations))
       (cc (- amount
              (first-denomination denominations))
           denominations)))))


(cc 100 (list 50 25 10 5 1))
;; 292

(cc 100 (list 100 50 20 10 5 2 1 0.5))
;; 104561 ... wow.


