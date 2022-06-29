#lang sicp
;; so slow implementation of fib
;; (define (fib n)
;;   (cond ((= n 0) 0)
;;         ((= n 1) 1)
;;         (else (+ (fib (- n 1))
;;                  (fib (- n 2))))))

;; iterative fib
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;; (fib 4)

; how many different ways can we make change of $1.00, given half-dollars
; quarters, dimes, nickels, and pennies?

;; the number of ways to change amount a using n kinds of coins equals
;1-> the number of ways to change amounta using all but the first kind of coin plus
; 2-> the number of ways to change amount a - d using all n kinds of coins where
; d is the denomination of the first kind of coin


; if a is exactly 0 , we should count that as 1 way to make change.
; if a is less than 0, we should count that as 0 ways to make change.
; if n is 0, we should count that as 0 ways to make change.

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  ; amount 0 ise para ustunu vermis oluyoruz 1 return
  (cond ((= amount 0 ) 1)
        ; amount - ye gidiyorsa yada coin paylasimi bittiyse 0 return
        ((or (< amount 0 ) (= kinds-of-coins 0 )) 0)
        ; else sunlari topla :
        ; + ayni amount ile kinds of coinsin 1 eksigini ve
        ; amountdan amountdan cikarilabilen coini cikar ve cc yi recur
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1 ) 1)
        ((= kinds-of-coins 2 )5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4 )25)
        ((= kinds-of-coins 5) 50)))

(define (f n)
  (cond ((< n 3) n)
        ((>= n 3) (+  (f (- n 1)) (* 2  (f (- n 2)) (* 3  (f (- n 3))))))))

(f 3)
