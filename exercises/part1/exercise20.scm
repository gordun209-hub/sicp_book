#lang sicp


(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

;; evaluate like normal-order

;; (gcd 206 40)
;; (if (= 40 0) 206 (gcd 40 (remainder 206 40)))
;; a = 40 b = (remainder 206 40)
;; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;;a = (remainder 206 40) b = (remainder 40 (remainder 206 40))
;; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder  ...
;;
